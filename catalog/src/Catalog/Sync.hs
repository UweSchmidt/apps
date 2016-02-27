{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
where

import           Catalog.Cmd
import           Catalog.FilePath
import           Control.Arrow ((***))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Function.Util
import           Data.ImageTree
import qualified Data.List as List
import           Data.Maybe
import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import           Data.RefTree
import           System.FilePath
import           System.Posix (FileStatus)
import qualified System.Posix as X

-- ----------------------------------------

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  trc $ "saveImgStore: save state to " ++ show p
  bs <- uses id J.encodePretty
  io $ if null p
       then L.putStrLn bs
       else L.writeFile p bs

loadImgStore :: FilePath -> Cmd ()
loadImgStore p = do
  trc $ "loadImgStore: load State from " ++ show p
  bs <- io $ L.readFile p
  case J.decode' bs of
    Nothing ->
      abort $ "loadImgStore: JSON input corrupted: " ++ show p
    Just st ->
      put st

-- ----------------------------------------

cwSyncFS :: Cmd ()
cwSyncFS = we >>= syncFS

-- ----------------------------------------

syncFS :: ObjId -> Cmd ()
syncFS = idSyncFS True

syncNode :: ObjId -> Cmd ()
syncNode = idSyncFS False

idSyncFS :: Bool -> ObjId -> Cmd ()
idSyncFS recursive i = getImgVal i >>= go
  where
    go e
      | isIMG e = do
          trcObj i "idSyncFS: syncing image"
          p  <- objid2path i
          ps <- collectImgCont i
          syncImg i p ps

      | isDIR e = do
          trcObj i "idSyncFS: syncing image dir"
          (do syncDirCont recursive i
              setDirSyncTime i
              checkEmptyDir i
            )
            `catchError`
            (\ _e ->
              do sync $ "fs dir not found " ++ show e
                 rmImgNode i
            )
          return ()
      | isROOT e = do
          trcObj i "idSyncFS: syncing root"
          idSyncFS recursive (e ^. theRootImgDir)

      | otherwise =
          return ()

syncDirCont :: Bool -> ObjId -> Cmd ()
syncDirCont recursive i = do
  trcObj i "syncDirCont: syncing entries in dir "
  (subdirs, imgfiles) <- collectDirCont i
  p  <- objid2path i

  cont <- objid2contNames i
  let lost = filter (`notElem` (subdirs ++ (map (fst . snd . head) imgfiles))) cont

  -- remove lost stuff
  mapM_ (remDirCont p) lost

  -- recurse into subdirs
  when recursive $
    mapM_ (syncSubDir p) subdirs

  -- sync the images
  mapM_ (syncImg i p) imgfiles
  where

    syncSubDir p n = do
      trc $ "syncSubDir: " ++ show i ++ ", " ++ show p ++ ", " ++ show n

      notex <- isNothing <$> getTree (entryAt new'i)
      when notex $
        mkImgDir i n >> return ()
      idSyncFS recursive new'i
      where
        new'i = mkObjId (p `snocPath` n)

    remDirCont p n = do
      trcObj i $ "remDirCont: remove entry " ++ show n ++ " from dir"

      -- will be done in rmImgNode
      -- adjustDirEntries (S.delete new'i) i
      rmImgNode new'i
      where
        new'i = mkObjId (p `snocPath` n)

collectImgCont :: ObjId -> Cmd ClassifiedNames
collectImgCont i = do
  nm <- getImgName   i
  ip <- getImgParent i
  cs <- snd <$> collectDirCont ip
  return $ concat . take 1 . filter (^. to head . _2 . _1 . to (== nm)) $ cs

collectDirCont :: ObjId -> Cmd ([Name], [ClassifiedNames])
collectDirCont i = do
  trcObj i "collectDirCont: group entries in dir "
  fp <- objid2path i >>= toFilePath
  es <- parseDirCont fp
  trc $ "collectDirCont: entries found " ++ show es

  let (others, rest) =
        List.partition (hasImgType (== IMGother)) es
  let (subdirs, rest2) =
        List.partition (hasImgType (== IMGimgdir)) rest
  let (imgfiles, rest3) =
        List.partition (hasImgType (`elem` [ IMGraw, IMGmeta, IMGjson
                                      , IMGjpg, IMGimg,  IMGcopy
                                      ])) rest2

  mapM_ (\ n -> sync $ "fs entry ignored " ++ show (fst n)) others
  realsubdirs <- filterM (isSubDir fp) subdirs

  trc $ "collectDirEntries: files ignored " ++ show rest3
  trc $ "collectDirEntries: subdirs "       ++ show realsubdirs
  trc $ "collectDirEntries: imgfiles "      ++ show imgfiles

  return ( realsubdirs ^.. traverse . _1
         , groupBy (^. _2 . _1) imgfiles
         )
  where
    isSubDir fp n =
      (const True <$> fsDirStat p)
      `catchError`
      (\ _ -> do sync $ "error caught, image dir expected " ++ show (show p)
                 return False
      )
      where
        p = fp </> (n ^. _1 . name2string)

type ClassifiedName  = (Name, (Name, ImgType))
type ClassifiedNames = [ClassifiedName]

syncImg :: ObjId -> Path -> ClassifiedNames -> Cmd ()
syncImg ip pp xs = do
  trcObj ip $ "syncImg: syncing img "

  -- new image ?
  notex <- isNothing <$> getTree (entryAt i)
  when notex $
    mkImg ip n >> return ()

  -- is there at least a raw image or a jpg?
  -- then update, else ignore image
  if or (xs ^.. traverse . _2 . _2 . to (`elem` [IMGraw, IMGjpg]))
    then do
      adjustImg (<> mkImgParts ps) i
      syncParts i pp
    else do
      p <- objid2path i
      sync $ "no raw or jpg found for " ++ show (show p) ++ ", parts: " ++ show xs
      rmImgNode i
  where
    i  = mkObjId (pp `snocPath` n)
    n  = xs ^. to head . _2 . _1
    ps = xs &  traverse %~ uncurry mkImgPart . (id *** snd)

syncParts :: ObjId -> Path -> Cmd ()
syncParts i pp = do
  trcObj i $ "syncParts: syncing img parts for "
  ps  <- getImgVals i (theParts . isoImgParts)
  ps' <- traverse syncPart ps
  adjustImg (const $ mkImgParts ps') i
  where
    syncPart p = do
      fsp <- toFilePath (pp `snocPath` (p ^. theImgName))
      ts  <- fsTimeStamp <$> fsFileStat fsp

      -- if file has changed, update timestamp and reset checksum
      return $
        if ts > p ^. theImgTimeStamp
        then p & theImgTimeStamp .~ ts
               & theImgCheckSum  .~ zeroCheckSum
        else p



checkEmptyDir :: ObjId -> Cmd ()
checkEmptyDir i = do
  nv <- getImgVal i
  when (nullImgDir nv) $ do
    p <- objid2path i
    sync $ "empty image dir ignored " ++ show (show p)
    rmImgNode i


fsStat :: String -> (FileStatus -> Bool) -> FilePath -> Cmd FileStatus
fsStat msg isFile p = do
  ex <- io $ X.fileExist p
  when (not ex) $
    abort $ "fs entry not found " ++ show p
  st <- io $ X.getFileStatus p
  when (not $ isFile st) $
    abort $ unwords ["fs entry not a", msg, show p]
  return st


fsDirStat :: FilePath -> Cmd FileStatus
fsDirStat = fsStat "directory" X.isDirectory

fsFileStat :: FilePath -> Cmd FileStatus
fsFileStat = fsStat "regular file" X.isRegularFile

parseDirCont :: FilePath -> Cmd [(Name, (Name, ImgType))]
parseDirCont p = do
  (es, jpgdirs)  <- classifyNames <$> scanDirCont p
  jss <- mapM
         (parseJpgDirCont p)                       -- process jpg subdirs
         (jpgdirs ^.. traverse . _1 . name2string) -- (map (fromName . fst) jpgdirs)
  return $ es ++ concat jss
  where
    classifyNames =
      List.partition (hasImgType (/= IMGjpgdir))  -- select jpg img subdirs
      .
      filter    (hasImgType (/= IMGboring))  -- remove boring stuff
      .
      map (\ n -> (mkName n, filePathToImgType n))

parseJpgDirCont :: FilePath -> FilePath -> Cmd [(Name, (Name, ImgType))]
parseJpgDirCont p d =
  classifyNames <$> scanDirCont (p </> d)
  where
    classifyNames =
      filter (\ n -> (n ^. _2 . _2) `elem` [IMGjpg, IMGcopy])
      .
      map (\ n -> (mkName (d </> n), filePathToImgType n))


scanDirCont :: FilePath -> Cmd [FilePath]
scanDirCont p0 = do
  trc $ "scanDirCont: reading dir " ++ show p0
  res <- io $ readDir p0
  trc $ "scanDirCont: result is " ++ show res
  return res
  where
    readDir :: FilePath -> IO [FilePath]
    readDir p = do
      s  <- X.openDirStream p
      xs <- readDirEntries s
      X.closeDirStream s
      return xs
      where
        readDirEntries s = do
          e1 <- X.readDirStream s
          if null e1
            then return []
            else do
              es <- readDirEntries s
              return (e1 : es)


hasImgType :: (ImgType -> Bool) -> (Name, (Name, ImgType)) -> Bool
hasImgType p (_, (_, t)) = p t

sync :: String -> Cmd ()
sync = logg verboseOn "sync"

-- ----------------------------------------
