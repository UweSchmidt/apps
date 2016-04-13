{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.ExifTool
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Data.Prim
import Data.ImageStore
import           Data.ImgTree
import           Data.MetaData

import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.Aeson as J

-- ----------------------------------------

getExifTool    :: FilePath -> Cmd MetaData
getExifTool f = do
  ex <- fileExist f
  if ex
    then
      execExifTool ["-groupNames", "-json"] f
      >>= (return . (^. from isoString))
      >>= buildMetaData
    else
      return mempty

execExifTool :: [String] -> FilePath -> Cmd String
execExifTool args f
  = execProcess "exiftool" (args ++ [f]) ""

buildMetaData :: ByteString -> Cmd MetaData
buildMetaData =
  either (abort . ("getExifTool: " ++)) return
  .
  J.eitherDecodeStrict'

writeMetaData :: FilePath -> MetaData -> Cmd ()
writeMetaData f m =
  runDry ("write metadata to file " ++ show f) $ do
    writeFileLB f (J.encodePretty' conf m)
  where
    conf = J.defConfig {J.confCompare = compare}

readMetaData :: FilePath -> Cmd MetaData
readMetaData f = do
  trc $ "readMetadata from " ++ show f
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
  unless px $ do             -- the dir for the exif file
    createDir $ takeDirectory p
  ts <- if px
        then getModiTime p   -- the time stamp
        else return mempty

  ps <- getImgVals i (theParts . isoImgParts)
  -- trcObj i $ "syncMetaData: syncing exif data " ++ show p ++ " " ++ show ps
  mapM_ (syncMD ip p ts) ps
  return ()

syncMD :: Path -> FilePath -> TimeStamp -> ImgPart -> Cmd ()
syncMD ip fp ts pt = do
  when ( ty `elem` [IMGraw, IMGimg, IMGmeta]  -- parts used by exif tool
         &&
         tw >= ts                             -- part has been changed
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
