{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
where

import           Catalog.Cmd
import           Catalog.FilePath
import           Control.Lens
import           Data.Function.Util
import           Data.ImageTree
import           Data.Prim
import           Data.RefTree

import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J

-- ----------------------------------------

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  trc $ "saveImgStore: save state to " ++ show p
  bs <- uses id J.encodePretty
  if null p
    then putStrLnLB    bs
    else writeFileLB p bs

loadImgStore :: FilePath -> Cmd ()
loadImgStore p = do
  trc $ "loadImgStore: load State from " ++ show p
  bs <- readFileLB p
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
              setSyncTime i
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
        partition (hasImgType (== IMGother)) es
  let (subdirs, rest2) =
        partition (hasImgType (== IMGimgdir)) rest
  let (imgfiles, rest3) =
        partition (hasImgType (`elem` [ IMGraw, IMGmeta, IMGjson
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
      dirExist $ fp </> (n ^. _1 . isoString)

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
               & theImgCheckSum  .~ mempty
        else p

checkEmptyDir :: ObjId -> Cmd ()
checkEmptyDir i = do
  nv <- getImgVal i
  when (isempty nv) $ do
    p <- objid2path i
    sync $ "empty image dir ignored " ++ show (show p)
    rmImgNode i


fsStat :: String -> (FilePath -> Cmd Bool) -> FilePath -> Cmd FileStatus
fsStat msg exists p = do
  ex <- exists p
  when (not ex) $
    abort $ "fs entry not found or not a " ++ msg ++ ": " ++ show (show p)
  getFileStatus p

fsDirStat :: FilePath -> Cmd FileStatus
fsDirStat = fsStat "directory" dirExist

fsFileStat :: FilePath -> Cmd FileStatus
fsFileStat = fsStat "regular file" fileExist

parseDirCont :: FilePath -> Cmd [(Name, (Name, ImgType))]
parseDirCont p = do
  (es, jpgdirs)  <- classifyNames <$> scanDirCont p
  jss <- mapM
         (parseJpgDirCont p)                       -- process jpg subdirs
         (jpgdirs ^.. traverse . _1 . isoString)
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
  res <- readDir p0
  trc $ "scanDirCont: result is " ++ show res
  return res

hasImgType :: (ImgType -> Bool) -> (Name, (Name, ImgType)) -> Bool
hasImgType p (_, (_, t)) = p t

sync :: String -> Cmd ()
sync = logg verboseOn "sync"

-- ----------------------------------------
