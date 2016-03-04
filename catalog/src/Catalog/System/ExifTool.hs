module Catalog.System.ExifTool
where

import           Catalog.Cmd
import           Control.Lens
import           Control.Lens.Util
import           Data.Prim
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
      >>= (return . (^. isoStringByteString))
      >>= buildMetaData
    else
      return emptyMetaData

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
      return emptyMetaData

-- ----------------------------------------

selectJSON :: FilePath -> [Name] -> Cmd MetaData
selectJSON f ns = do
  (^. selectByNames ns) <$> readMetaData f

lookupJSON :: FilePath -> [Name] -> Cmd Text
lookupJSON f ns =
  lookupByNames ns <$> readMetaData f

-- ----------------------------------------
