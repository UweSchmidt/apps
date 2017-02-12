{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ExifTool
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Catalog.FilePath
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

getExifTool    :: FilePath -> Cmd MetaData
getExifTool f = do
  ex <- fileExist f

  if ex && matchRE imgExtExpr f
    then
      ( do md <- (^. from isoString) <$> execExifTool ["-groupNames", "-json"] f
           buildMetaData md
      )
      `catchError`
      ( \ e -> do
          warn $ "exiftool failed for " ++ show f ++ ", error: " ++ show e
          return mempty
      )
    else
      return mempty

execExifTool :: [String] -> FilePath -> Cmd String
execExifTool args f = do
  trc $ unwords ["exiftool", show (args ++ [f]), ""]
  execProcess "exiftool" (args ++ [f]) ""

buildMetaData :: ByteString -> Cmd MetaData
buildMetaData =
  either (abort . ("getExifTool: " ++)) return
  .
  J.eitherDecodeStrict'

writeMetaData :: FilePath -> MetaData -> Cmd ()
writeMetaData f m =
  runDry ("write metadata to file " ++ show f) $
    writeFileLB f (J.encodePretty' conf m)
  where
    conf = J.defConfig {J.confCompare = compare}

readMetaData :: FilePath -> Cmd MetaData
readMetaData f = do
  trc $ "readMetadata from " ++ show f
  whenM (do ex <- fileExist f
            unless ex $
              warn $ "readMetaData: metadata file not found: " ++ show f
            return ex
        ) $ do
      bs <- readFileLB f
      case J.decode' bs of
        Nothing -> do
          warn $ "readMetaData: no metadata, JSON input corrupted: " ++ show f
          return mempty
        Just m ->
          return m

getMetaData :: ObjId -> Cmd MetaData
getMetaData i =
  objid2path i >>= exifPath >>= readMetaData

-- ----------------------------------------

forceSyncAllMetaData :: ObjId -> Cmd ()
forceSyncAllMetaData i = local (envForceMDU .~ True) (syncAllMetaData i)

syncAllMetaData :: ObjId -> Cmd ()
syncAllMetaData i0 = do
  p <- objid2path i0
  verbose $ "syncAllMetaData for: " ++ show (i0, p)

  foldMT imgA dirA rootA colA i0
  where
    imgA i ps _md = syncMetaData' i (ps ^. isoImgParts)

    -- traverse the DIR
    dirA go _i es _ts = traverse_ go (es ^. isoDirEntries)

    -- we only need to traverse the DIR hierachy
    rootA go _i dir _col = go dir

    -- for a COL the col img and the entries must be traversed
    colA go _i _md im _be es = do
      traverse_ (go . fst) im
      traverse_ go' es
        where
          go' = colEntry (\ i' _name -> go i') go

-- i must be an objid pointing to am ImgNode
-- else this becomes a noop

syncMetaData :: ObjId -> Cmd ()
syncMetaData i = do
  ps <- getImgVals i (theParts . isoImgParts)
  unless (null ps) $
    syncMetaData' i ps

syncMetaData' :: ObjId -> [ImgPart] -> Cmd ()
syncMetaData' i ps = do
  ip <- objid2path i

  p  <- exifPath ip          -- the exif file path
  px <- fileExist p
  unless px $                -- the dir for the exif file
    createDir $ takeDirectory p

  ts <- if px
        then getModiTime p   -- the time stamp
        else return mempty

  fu <- view envForceMDU
  let update = fu || (ts <= ps ^. traverse . theImgTimeStamp)

  trc $
    "syncMetaData: syncing exif data "
    ++ show p ++ " " ++ show ps ++ " " ++ i ^. isoString

  -- collect meta data from raw and xmp parts
  when update $ do
    mapM_ (syncMD isRawMeta ip p) ps

    -- if neither raw nor xmp there, collect meta from jpg files
    unless (has (traverse . isA isRawMeta) ps) $
      mapM_ (syncMD isJpg ip p) ps

syncMD :: (ImgPart -> Bool) ->
          Path -> FilePath -> ImgPart -> Cmd ()
syncMD p ip fp pt =
  when ( p pt ) $ do    -- ty `elem` [IMGraw, IMGimg, IMGmeta]  -- parts used by exif tool
    sp <- toFilePath (substPathName tn ip)
    verbose $ "syncMetaData: syncing with " ++ show sp
    m1 <- readMetaData fp
    m2 <- filterMetaData ty <$> getExifTool sp
    writeMetaData fp (m2 <> m1)
  where
    ty = pt ^. theImgType
    tn = pt ^. theImgName

isRawMeta :: ImgPart -> Bool
isRawMeta pt = pt ^. theImgType `elem` [IMGraw, IMGmeta]

isJpg :: ImgPart -> Bool
isJpg pt = pt ^. theImgType == IMGjpg

-- ----------------------------------------

-- build a file path from an internal image path
--
-- "/archive/photos/2016/emil"
-- -->
-- "<mountpath>/cache/exif-meta/photos/2016/emil.json"

exifPath :: Path -> Cmd FilePath
exifPath ip = do
  mp <- use theMountPath
  return $
    mp ++ ps'exifcache ++ tailPath ip ^. isoString ++ ".json"

-- ----------------------------------------
