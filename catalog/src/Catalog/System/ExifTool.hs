{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ExifTool
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.System.IO ( fileExist )
import qualified Data.Aeson as J
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------
--
-- the low level ops
-- working on file system paths

-- add or update metadata of an image
-- by extracting exif data form a raw image
-- a jpg or a sidecar and merge it with given
-- metadata

getMDpart :: (ImgType -> Bool)
          -> Path
          -> ImgPart
          -> Cmd MetaData
getMDpart p ip pt
  | p $ pt ^. theImgType  = do
      sp <- path2SysPath (substPathName tn ip)
      trc $ "getMDpart: update metadata with " ++ show sp
      m2 <- filterMetaData ty <$> getExifTool sp
      trc $ "getMDpart: metadata= " ++ show m2
      return m2
  | otherwise =
      return mempty
  where
    ty = pt ^. theImgType
    tn = pt ^. theImgName


setMD :: (ImgType -> Bool)
      -> ObjId
      -> [ImgPart]
      -> Cmd ()
setMD p i ps = do
  ip  <- objid2path i

  -- get old metadata
  md0 <- getMetaData i

  -- merge metadata of all image parts with old metadata
  md1 <- ((<> md0) . mconcat) <$> mapM (getMDpart p ip) ps

  if md1 /= md0
     ||
     isempty (getEXIFUpdateTime md0)
    then
      -- something has changed since last update
      -- so add timestamp and store new metadata
      do md2 <- flip setEXIFUpdateTime md1 <$> now
         adjustMetaData (const md2) i
         verbose $
           "setMD: update exif data for " ++ show ip
    else
         trc $
           "setMD: no change in exif data " ++ show ip

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
  verbose $ unwords ["exiftool", show (args ++ [sp ^. isoFilePath]), ""]
  execProcess "exiftool" (args ++ [sp ^. isoFilePath]) ""

-- ----------------------------------------

buildMetaData :: ByteString -> Cmd MetaData
buildMetaData =
  either (abort . ("getExifTool: " ++)) return
  .
  J.eitherDecodeStrict'

-- ----------------------------------------

forceSyncAllMetaData :: ObjId -> Cmd ()
forceSyncAllMetaData i = local (envForceMDU .~ True) (syncAllMetaData i)

syncAllMetaData :: ObjId -> Cmd ()
syncAllMetaData i0 = do
  p <- objid2path i0
  trc $ "syncAllMetaData for: " ++ show (i0, p)

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
    -- redundant: getRating looks up descr:rating and xmp:rating
    -- syncRating i


syncMetaData' :: ObjId -> [ImgPart] -> Cmd ()
syncMetaData' i ps = do
  ts <- getEXIFUpdateTime <$> getMetaData i
  fu <- view envForceMDU
  let update = fu || (ts < ps ^. traverse . theImgTimeStamp)

  -- trc $ "syncMetadata: " ++ show (ts, ps ^. traverse . theImgTimeStamp, update)

  -- collect meta data from raw and xmp parts
  when update $ do
    setMD isRawMeta i ps

    -- if neither raw, png, ... nor xmp there, collect meta from jpg files
    unless (has (traverse . theImgType . isA isRawMeta) ps) $
      setMD isJpg i ps

{-
-- rating is stored in image node, not in exif data file
-- but rating is imported from LR xmp file with keyword "XMP:Rating"
-- if rating in .xmp is set, but not rating in image node
-- the rating is copied to the image node
--
-- so for rating, there's no need to read the exif file
-- except when syncing exif data

syncRating :: ObjId -> Cmd ()
syncRating i = do
  md1 <- getMetaData i
  when (T.null $ md1 ^. metaDataAt descrRating) $ do
    -- rating not yet set in catalog
    -- try to take rating from .xmp file
    md2 <- getMetaData i
    unless (T.null $ md2 ^. metaDataAt xmpRating) $
      adjustMetaData ((mkRating $ getRating md2) <>) i
-}

-- ----------------------------------------
