{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ExifTool
where

import           Catalog.Cmd.Basic
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
  verbose $ unwords ["exiftool", show (args ++ [f]), ""]
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
  -- trc $ "readMetadata from " ++ show f
  ex <- fileExist f
  if ex
    then do
      bs <- readFileLB f
      case J.decode' bs of
        Nothing ->
          abort $ "readMetaData: JSON input corrupted: " ++ show f
        Just m ->
          return m
    else
      return mempty

getMetaData :: ObjId -> Cmd MetaData
getMetaData i =
  objid2path i >>= exifPath >>= readMetaData

-- ----------------------------------------

syncMetaData :: ObjId -> Cmd ()
syncMetaData i = do
  ip <- objid2path i

  p  <- exifPath ip          -- the exif file path
  px <- fileExist p
  unless px $                -- the dir for the exif file
    createDir $ takeDirectory p
  ts <- if px
        then getModiTime p   -- the time stamp
        else return mempty

  ps <- getImgVals i (theParts . isoImgParts)
  fu <- view envForceMDU

  verbose $
    "syncMetaData: syncing exif data "
    ++ show p ++ " " ++ show ps ++ " " ++ i ^. isoString

  -- collect meta data from raw and xmp parts
  mapM_ (syncMD isRawMeta fu ip p ts) ps

  -- if neither raw nor xmp there, collect meta from jpg files
  unless (has (traverse . isA isRawMeta) ps) $
    mapM_ (syncMD isJpg fu ip p ts) ps

syncMD :: (ImgPart -> Bool) -> Bool -> Path -> FilePath -> TimeStamp -> ImgPart -> Cmd ()
syncMD p fu ip fp ts pt =
  when ( p pt    -- ty `elem` [IMGraw, IMGimg, IMGmeta]  -- parts used by exif tool
         &&
         (fu || tw >= ts)                     -- force update or part has been changed
       ) $ do
    sp <- toFilePath (substPathName tn ip)
    trc $ "syncMD: syncing with " ++ show sp
    m1 <- readMetaData fp
    m2 <- filterMetaData ty <$> getExifTool sp
    writeMetaData fp (m2 <> m1)
  where
    ty = pt ^. theImgType
    tw = pt ^. theImgTimeStamp
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
-- "<mountpath>/exif-meta/photos/2016/emil.json"

exifPath :: Path -> Cmd FilePath
exifPath ip = do
  mp <- use theMountPath
  return $
    mp </> "exif-meta" ++ tailPath ip ^. isoString ++ ".json"

-- ----------------------------------------
