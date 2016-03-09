module Catalog.System.ExifTool
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Data.Prim
import           Data.ImageTree
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

-- ----------------------------------------

selectJSON :: FilePath -> [Name] -> Cmd MetaData
selectJSON f ns = do
  (^. selectByNames ns) <$> readMetaData f

lookupJSON :: FilePath -> [Name] -> Cmd Text
lookupJSON f ns =
  lookupByNames ns <$> readMetaData f

getMetaData :: ObjId -> Cmd MetaData
getMetaData i = do
  pts <- getImgVals i theParts
  let jsonName = pts ^. thePartNames IMGjson
  jp  <- substPathName jsonName <$> objid2path i
  md <- toFilePath jp >>= readMetaData
  -- trcObj i $ "getmetadata: " ++ show md
  return md

-- ----------------------------------------
