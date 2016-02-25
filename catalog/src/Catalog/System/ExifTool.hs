module Catalog.System.ExifTool
where

import Catalog.Cmd
import Control.Lens
import Control.Lens.Util
-- import Data.ImgAction
import Data.Prim.Prelude
import Data.ImageTree
import Data.MetaData
import Data.Aeson (eitherDecodeStrict')
import qualified System.Posix as X
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.Aeson as J

-- ----------------------------------------

getExifTool    :: FilePath -> Cmd MetaData
getExifTool f = do
  ex <- io $ X.fileExist f
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
  eitherDecodeStrict'

writeMetaData :: FilePath -> MetaData -> Cmd ()
writeMetaData f m = do
  trc $ "writeMetadata to file " ++ f
  io $ LB.writeFile f (J.encodePretty m)

readMetaData :: FilePath -> Cmd MetaData
readMetaData f = do
  trc $ "readMetadata from " ++ show f
  ex <- io $ X.fileExist f
  if ex
    then do
      bs <- io $ LB.readFile f
      case J.decode' bs of
        Nothing ->
          abort $ "readMetaData: JSON input corrupted: " ++ show f
        Just m ->
          return m
    else
      return emptyMetaData

filterMetaData :: ImgType -> MetaData -> MetaData
filterMetaData IMGraw  m = m
filterMetaData IMGmeta m = m
filterMetaData IMGjpg  m = m
filterMetaData _       _ = emptyMetaData

-- ----------------------------------------

mkPar :: String -> String
mkPar s = "(" ++ s ++ ")"

mkAlt :: [String] -> String
mkAlt xs = mkPar $ intercalate "|" $ map mkPar xs

type AttrGroup = (String, [String])

attrGroups2regex :: [AttrGroup] -> Regex
attrGroups2regex =
  parseRegex .
  mkAlt .
  map (\ (px, attr) -> (px ++ mkAlt attr))

reRaw :: Regex
reRaw = attrGroups2regex
  [ attrExif
  , attrComposite
  , attrMaker
  ]

attrComposite :: AttrGroup
attrComposite =
  ( "Composite"
  , [ "Aperture"
    , "AutoFocus"
    , "ImageSize"
    , "LensID"
    , "LensSpec"
    , "Megapixels"
    , "ShutterSpeed"
    , "SubSecDateTimeOriginal"
    , "DOF"
    , "FOV"
    , "FocalLength35efl"
    , "HyperfocalDistance"
    , "LightValue"
    ]
  )

attrExif :: AttrGroup
attrExif =
  ( "EXIF"
  , [ "Make"
    , "Model"
    , "Orientation"
    , "Artist"
    , "ImageWidth"
    , "ImageHeight"
    , "BitsPerSample"
    , "Copyright"
    , "ExposureTime"
    , "FNumber"
    , "ExposureProgram"
    , "ISO"
    , "CreateDate"
    , "ExposureCompensation"
    , "MaxApertureValue"
    , "MeteringMode"
    , "Flash"
    , "FocalLength"
    , "UserComment"
    , "ExposureMode"
    , "WhiteBalance"
    , "FocalLengthIn35mmFormat"
    , "GPSVersionID"
    ]
  )

attrMaker :: AttrGroup
attrMaker =
  ( "MakerNotes"
  , [ "Quality"
    , "FocusMode"
    , "SerialNumber"
    , "ColorSpace"
    , "TimeZone"
    , "DaylightSavings"
    , "ShootingMode"
    , "ShutterCount"
    ]
  )
