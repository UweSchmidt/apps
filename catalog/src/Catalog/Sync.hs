{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
where

import Catalog.Cmd
import Catalog.FilePath        (filePathToImgType)
import Catalog.System.ExifTool (syncMetaData)
import Data.ImgTree
import Data.Prim

-- ----------------------------------------

cwSyncFS :: Cmd ()
cwSyncFS = we >>= syncFS

-- ----------------------------------------

-- sync the whole photo archive with disk contents

syncFS :: ObjId -> Cmd ()
syncFS = idSyncFS True

-- sync a single entry of the archive (image or dir) with disk contents

syncNode :: ObjId -> Cmd ()
syncNode = idSyncFS False

-- sync a dir tree, given as path, in the archive with disk contents

syncDir :: Path -> Cmd ()
syncDir p = do
  (i, n) <- getIdNode "syncDir: image dir not found:" p
  unless (isDIR n) $
    abort $ "syncDIR: path isn't an image dir: " ++ show (p ^. isoString)
  idSyncFS True i

-- ----------------------------------------
-- the work horse

idSyncFS :: Bool -> ObjId -> Cmd ()
idSyncFS recursive i = getImgVal i >>= go
  where
    go e
      | isIMG e = do
          -- trcObj i "idSyncFS: syncing image"
          p  <- objid2path i
          ps <- collectImgCont i
          syncImg i p ps

      | isDIR e = do
          fp <- objid2path i >>= toFilePath
          ex <- dirExist fp
          if ex
            then do
              syncDirCont recursive i
              setSyncTime i
              checkEmptyDir i
            else do
              verbose $ "sync: fs dir not found: " ++ show fp
              rmImgNode i

      | isROOT e = do
          -- trcObj i "idSyncFS: syncing root"
          idSyncFS recursive (e ^. theRootImgDir)

      | otherwise =
          return ()

syncDirCont :: Bool -> ObjId -> Cmd ()
syncDirCont recursive i = do
  -- trcObj i "syncDirCont: syncing entries in dir "
  (subdirs, imgfiles) <- collectDirCont i
  -- trc $ "syncDirCont: " ++ show (subdirs, imgfiles)
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
      -- trc $ "syncSubDir: " ++ show p ++ "/" ++ show n
      whenM (isNothing <$> getTree (entryAt new'i)) $
        void $ mkImgDir i n

      idSyncFS recursive new'i
      where
        new'i = mkObjId (p `snocPath` n)

    remDirCont p n = do
      -- trcObj i $ "remDirCont: remove entry " ++ show n ++ " from dir"
      rmRec new'i
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
  -- trcObj i "collectDirCont: group entries in dir "
  fp <- objid2path i >>= toFilePath
  es <- parseDirCont fp
  -- trc $ "collectDirCont: entries found " ++ show es

  let (others, rest) =
        partition (hasImgType (== IMGother)) es
  let (subdirs, rest2) =
        partition (hasImgType (== IMGimgdir)) rest
  let (imgfiles, rest3) =
        partition (hasImgType (`elem` [ IMGraw, IMGmeta, IMGjson
                                      , IMGjpg, IMGimg,  IMGcopy
                                      , IMGtxt
                                      ])) rest2

  mapM_ (\ n -> verbose $ "sync: fs entry ignored " ++ show (fst n)) others
  realsubdirs <- filterM (isSubDir fp) subdirs

  unless (null rest3) $
    trc $ "collectDirCont: files ignored " ++ show rest3
  unless (null realsubdirs) $
    trc $ "collectDirCont: subdirs "       ++ show realsubdirs
  unless (null imgfiles) $
    trc $ "collectDirCont: imgfiles "      ++ show imgfiles

  return ( realsubdirs ^.. traverse . _1
         , partitionBy (^. _2 . _1) imgfiles
         )
  where
    isSubDir fp n =
      dirExist $ fp </> (n ^. _1 . isoString)

type ClassifiedName  = (Name, (Name, ImgType))
type ClassifiedNames = [ClassifiedName]

syncImg :: ObjId -> Path -> ClassifiedNames -> Cmd ()
syncImg ip pp xs = do
  -- new image ?
  whenM (isNothing <$> getTree (entryAt i)) $
    void $ mkImg ip n

  -- trcObj i $ "syncImg: "

  -- is there at least a jpg or  raw image or a txt (something, that can be shown)?
  -- then update, else ignore the entry
  if has (traverse . _2 . _2 . isA (`elem` [IMGraw, IMGimg, IMGjpg, IMGtxt])) xs
    then do
      adjustImg (<> mkImgParts ps) i
      syncParts i pp
    else do
      p <- objid2path i
      verbose $ "sync: no raw, jpg or txt found for " ++ show (show p) ++ ", parts: " ++ show xs
      rmImgNode i
  where
    i  = mkObjId (pp `snocPath` n)
    n  = xs ^. to head . _2 . _1
    ps = xs &  traverse %~ uncurry mkImgPart . (id *** snd)

syncParts :: ObjId -> Path -> Cmd ()
syncParts i pp = do
  -- trcObj i $ "syncParts: syncing img parts for "
  ps  <- getImgVals i (theParts . isoImgParts)
  ps' <- traverse syncPart ps
  adjustImg (const $ mkImgParts ps') i
  syncMetaData i
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
  whenM (isempty <$> getImgVal i) $ do
    p <- objid2path i
    verbose $ "sync: empty image dir ignored " ++ show (show p)
    rmImgNode i


fsStat :: String -> (FilePath -> Cmd Bool) -> FilePath -> Cmd FileStatus
fsStat msg exists p = do
  ex <- exists p
  unless ex $
    abort $ "fs entry not found or not a " ++ msg ++ ": " ++ show (show p)
  getFileStatus p

fsDirStat :: FilePath -> Cmd FileStatus
fsDirStat = fsStat "directory" dirExist

fsFileStat :: FilePath -> Cmd FileStatus
fsFileStat = fsStat "regular file" fileExist

parseDirCont :: FilePath -> Cmd [(Name, (Name, ImgType))]
parseDirCont p = do
  (es, jpgdirs)  <- classifyNames <$> scanDirCont p
  -- trc $ "parseDirCont: " ++ show (es, jpgdirs)
  jss <- mapM
         (parseJpgDirCont p)                       -- process jpg subdirs
         (jpgdirs ^.. traverse . _1 . isoString)
  -- trc $ "parseDirCont: " ++ show jss
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
      filter (\ n -> (n ^. _2 . _2) == IMGjpg)
      .
      map (\ n -> let dn = d </> n
                  in (mkName dn, filePathToImgType dn)
          )


scanDirCont :: FilePath -> Cmd [FilePath]
scanDirCont p0 = do
  -- trc $ "scanDirCont: reading dir " ++ show p0
  res <- readDir p0
  -- trc $ "scanDirCont: result is " ++ show res
  return res

hasImgType :: (ImgType -> Bool) -> (Name, (Name, ImgType)) -> Bool
hasImgType p (_, (_, t)) = p t

-- ----------------------------------------
