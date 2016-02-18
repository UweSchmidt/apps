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
import           Data.ImageStore
import           Data.ImageTree
import           Data.List ({-intercalate,-} partition)
import           Data.Maybe
import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import           Data.RefTree
import qualified Data.Set as S
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import qualified System.Posix as X
-- import           Control.Lens.Util
-- import           Control.Applicative
-- import qualified Data.Aeson as J
-- import           Data.Aeson hiding (Object, (.=))
-- import qualified Data.ByteString as B
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import           Data.Maybe
-- import           Data.Prim.CheckSum
-- import Data.ImageTree
-- import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)

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

idSyncFS :: ObjId -> Cmd ()
idSyncFS i = getImgVal i >>= go
  where
    go e
      | isIMG e = do
          trcObj i "idSyncFS: syncing image"
          p  <- id2path i
          ps <- collectImgCont i
          syncImg i p ps

      | isDIR e = do
          trcObj i "idSyncFS: syncing image dir"
          (do s <- id2path i >>= toFilePath >>= fsDirStat
              when (fsTimeStamp s > e ^. theDirTimeStamp) $
                do trc "idSyncFS: dir has changed since last sync"
                   syncDirCont i
                   adjustDirTimeStamp (const $ fsTimeStamp s) i
              checkEmptyDir i
            )
            `catchError`
            (\ _e ->
              do warn $ "idSyncFS: fs dir not found " ++ show e
                 rmImgNode i
            )
          return ()

      | otherwise =
          return ()

syncDirCont :: ObjId -> Cmd ()
syncDirCont i = do
  trcObj i "syncDirCont: syncing entries in dir "
  (subdirs, imgfiles) <- collectDirCont i
  p  <- id2path i

  cont <- id2contNames i
  let lost = filter (`notElem` (subdirs ++ (map (fst . snd . head) imgfiles))) cont

  mapM_ (remDirCont i p) lost
  mapM_ (syncSubDir i p) subdirs
  mapM_ (syncImg    i p) imgfiles

remDirCont :: ObjId -> Path -> Name -> Cmd ()
remDirCont i p n = do
  trcObj i $ "remDirCont: remove entry " ++ show n ++ " from dir"
  adjustDirEntries (S.delete new'i) i
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
  fp <- id2path i >>= toFilePath
  es <- parseDirCont fp
  trc $ "collectDirCont: entries found " ++ show es

  let (others, rest) =
        partition (hasImgType (== IMGother)) es
  let (subdirs, rest2) =
        partition (hasImgType (== IMGimgdir)) rest
  let (imgfiles, rest3) =
        partition (hasImgType (`elem` [ IMGraw, IMGmeta, IMGjson
                                      , IMGjpg, IMGimg,  IMGcopy
                                      ])) rest2

  mapM_ (\ n -> warn $ "collectDirCont: other entry ignored " ++ show (fst n)) others
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
      (\ _ -> do warn $ "collectDirCont: error catched, not an image dir: " ++ show p
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
      p <- id2path i
      warn $ "syncImg: no raw or jpg found in " ++ show p ++ ", parts: " ++ show xs
      rmImgNode i
  where
    i  = mkObjId (pp `snocPath` n)
    n  = xs ^. to head . _2 . _1
    ps = xs &  traverse %~ uncurry mkImgPart . (id *** snd)

syncSubDir :: ObjId -> Path -> Name -> Cmd ()
syncSubDir ip pp n = do
  trc $ "syncSubDir: " ++ show ip ++ ", " ++ show pp ++ ", " ++ show n
  notex <- isNothing <$> getTree (entryAt new'i)
  when notex $
    mkImgDir ip n >> return ()
  idSyncFS new'i
  where
    new'i = mkObjId (pp `snocPath` n)

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
    p <- id2path i
    warn $ "checkEmptyDir: image dir empty, will be removed " ++ show p
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
      partition (hasImgType (/= IMGjpgdir))  -- select jpg img subdirs
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

-- ----------------------------------------


ccc :: IO (Either Msg (), ImgStore, Log)
ccc = runCmd $ do
  mountPath <- io X.getWorkingDirectory
  initImgStore "archive" "collections" mountPath
  trcCmd cwnPath >> trcCmd cwnLs >> return ()
  saveImgStore ""

  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)
  cwSet refImg >> trcCmd cwnPath >> trcCmd cwnType >> return ()

  cwe <- we
  refDir1 <- mkImgDir cwe "emil"
  cwSet refDir1 >> trcCmd cwnPath >> trcCmd cwnType >> trcCmd cwnFilePath >> return ()

  cwe' <- we
  pic1 <- mkImg cwe' "pic1"
  pic2 <- mkImg cwe' "pic2"
  trcCmd cwnLs >> return ()

  cwSet pic2 >> trcCmd cwnPath >> trcCmd cwnType >> trcCmd cwnLs >> return ()
  cwe'' <- we
  (mkImg cwe'' "xxx" >> return ()) `catchError` (\ _ -> return ()) -- error

  cwRoot >> trcCmd cwnType >> trcCmd cwnLs >> trcCmd cwnPath >> return ()
--  trcCmd (fromFilePath "/home/uwe/haskell/apps/catalog/emil") >> return ()
  saveImgStore ""
  rmImgNode pic1
  rmImgNode pic2
  rmImgNode refDir1

  idSyncFS refImg
  saveImgStore ""
  trc "save state to c1.json"
  saveImgStore "c1.json"
  trc "load state from c1.json"
  loadImgStore "c1.json"
  saveImgStore ""
  (formatImages <$> listImages) >>= io . putStrLn
  cwnListPaths >>= trc
  cwnListNames >>= trc
