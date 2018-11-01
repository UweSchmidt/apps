{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ExifTool
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.System.IO ( createDir
                                   , fileExist
                                   , getModiTime
                                   , readFileLB
                                   , writeFileLB
                                   )
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim
import qualified Data.Text as T

-- ----------------------------------------
--
-- the low level ops
-- working on file system paths


-- update MetaData with exif data
-- contained in the .nef, .png, .jpg, .xmp files

syncMD :: (ImgPart -> Bool)
       -> Path -> SysPath -> ImgPart -> Cmd ()
syncMD p ip fp pt =
  when ( p pt ) $ do
  m1 <- readMetaData fp
  m2 <- addMD (const True) ip pt m1
  writeMetaData fp m2

addMD :: (ImgPart -> Bool)
      -> Path -> ImgPart
      -> MetaData -> Cmd MetaData
addMD p ip pt m1
  | p pt = do    -- ty `elem` [IMGraw, IMGimg, IMGmeta]
      sp <- path2SysPath (substPathName tn ip)
      verbose $ "syncMD: syncing with " ++ show sp
      m2 <- filterMetaData ty <$> getExifTool sp
      return (m2 <> m1)
  | otherwise =
      return m1
  where
    ty = pt ^. theImgType
    tn = pt ^. theImgName

-- ----------------------------------------

getExifTool    :: SysPath -> Cmd MetaData
getExifTool sp = do
  ex <- fileExist sp
  if ex
    then
      ( do md <- (^. from isoString) <$>
                 execExifTool ["-groupNames", "-json"] sp
           buildMetaData md
      )
      `catchError`
      ( \ e -> do
          warn $ "exiftool failed for " ++ show sp ++ ", error: " ++ show e
          return mempty
      )
    else do
      warn $ "exiftool: file not found: " ++ show sp
      return mempty

execExifTool :: [String] -> SysPath -> Cmd String
execExifTool args sp = do
  trc $ unwords ["exiftool", show (args ++ [sp ^. isoFilePath]), ""]
  execProcess "exiftool" (args ++ [sp ^. isoFilePath]) ""

writeMetaData :: SysPath -> MetaData -> Cmd ()
writeMetaData f m =
  runDry ("write metadata to file " ++ show f) $
    writeFileLB f (J.encodePretty' conf m)
  where
    conf = J.defConfig {J.confCompare = compare}

readMetaData :: SysPath -> Cmd MetaData
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

-- ----------------------------------------

buildMetaData :: ByteString -> Cmd MetaData
buildMetaData =
  either (abort . ("getExifTool: " ++)) return
  .
  J.eitherDecodeStrict'

getMetaData :: ObjId -> Cmd MetaData
getMetaData i = do
  md1 <- getImgVals i theMetaData
  md2 <- objid2path i >>= path2ExifSysPath >>= readMetaData
  return $ md1 `mergeMD` md2

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
      traverse_ (go . (\(ImgRef i' _name) -> i')) im
      traverse_ go' es
        where
          go' = colEntry' (go . _iref) go

-- i must be an objid pointing to am ImgNode
-- else this becomes a noop

syncMetaData :: ObjId -> Cmd ()
syncMetaData i = do
  ps <- getImgVals i (theParts . isoImgParts)
  unless (null ps) $ do
    syncMetaData' i ps
    syncRating i


syncMetaData' :: ObjId -> [ImgPart] -> Cmd ()
syncMetaData' i ps = do
  ip <- objid2path i

  sp <- path2ExifSysPath ip
  px <- fileExist sp
  unless px $                -- the dir for the exif file
    createDir (takeDirectory <$> sp)

  ts <- if px
        then getModiTime sp   -- the time stamp
        else return mempty

  fu <- view envForceMDU
  let update = fu || (ts <= ps ^. traverse . theImgTimeStamp)

  trc $
    "syncMetaData: syncing exif data "
    ++ show sp ++ " " ++ show ps ++ " " ++ i ^. isoString

  -- collect meta data from raw and xmp parts
  when update $ do
    mapM_ (syncMD isRawMeta ip sp) ps

    -- if neither raw, png, ... nor xmp there, collect meta from jpg files
    unless (has (traverse . isA isRawMeta) ps) $
      mapM_ (syncMD isJpg ip sp) ps

isRawMeta :: ImgPart -> Bool
isRawMeta pt = pt ^. theImgType `elem` [IMGraw, IMGimg, IMGmeta]

isJpg :: ImgPart -> Bool
isJpg pt = pt ^. theImgType == IMGjpg


-- rating is stored in image node, not in exif data file
-- but rating is imported from LR xmp file with keyword "XMP:Rating"
-- if rating in .xmp is set, but not rating in image node
-- the rating is copied to the image node
--
-- so for rating, there's no need to read the exif file
-- except when syncing exif data

syncRating :: ObjId -> Cmd ()
syncRating i = do
  md1 <- getImgVals i theMetaData
  when (T.null $ md1 ^. metaDataAt descrRating) $ do
    -- rating not yet set in catalog
    -- try to take rating from .xmp file
    md2 <- getMetaData i
    unless (T.null $ md2 ^. metaDataAt xmpRating) $
      adjustMetaData ((mkRating $ getRating md2) <>) i

-- ----------------------------------------
