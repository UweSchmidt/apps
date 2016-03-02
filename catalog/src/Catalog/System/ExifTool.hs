module Catalog.System.ExifTool
where

import           Catalog.Cmd
import           Control.Lens
import           Control.Lens.Util
import           Data.Prim.Name
import           Data.Prim.PathId
import           Data.Prim.Prelude
import           Data.ImageTree
import           Data.MetaData
import           Data.Aeson (eitherDecodeStrict')
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
writeMetaData f m =
  runDry ("write metadata to file " ++ show f) $ do
    io $ LB.writeFile f (J.encodePretty' conf m)
  where
    conf = J.defConfig {J.confCompare = compare}

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
filterMetaData IMGraw  m = m ^. selectByRegex reRaw
filterMetaData IMGmeta m = m ^. selectByRegex reXmp
filterMetaData IMGimg  m = m ^. selectByRegex reRaw
filterMetaData _       _ = emptyMetaData

-- ----------------------------------------

lookupMeta' :: FilePath -> [Name] -> Cmd MetaData
lookupMeta' f ns = do
  md <- readMetaData f
  return (md ^. selectMetaData (`elem` ns))

lookupMeta :: FilePath -> String -> Cmd MetaData
lookupMeta f n0 = do
  -- f <- objid2path i >>= (return . (subst))>>= toFilePath
  trc $ show (f, n)
  lookupMeta' f n
  where
    n = px2a n0

-- ----------------------------------------

mkPar :: String -> String
mkPar s = "(" ++ s ++ ")"

mkAlt :: [String] -> String
mkAlt xs = mkPar $ intercalate "|" $ map mkPar xs

type AttrGroup = (String, [String])

attrGroups2regex :: [AttrGroup] -> RegexText
attrGroups2regex =
  parseRegex' .
  mkAlt .
  map (\ (px, attr) -> (px ++ ":" ++ mkAlt attr))

reRaw :: RegexText
reRaw = attrGroups2regex
  [ attrExif
  , attrComposite
  , attrMaker
  , attrFile
  ]

reXmp :: RegexText
reXmp = attrGroups2regex
  [ attrComposite
  , attrXmp
  ]

-- ----------------------------------------

px2a :: String -> [Name]
px2a s = map mkName ag20
{-}
  case ag20 of
    [x1] -> mkName x1
    []   -> emptyName
    xs   -> error $ "ambigious name abreviation " ++ show s ++ " matches " ++ show xs
-- -}
  where
    (g, n) | null n'   = ("", g')
           | otherwise = (g', tail n')
      where
        (g', n') = span (/= ':') s

    filterNotNull =
      filter (not . null .snd)

    ag20 = concatMap (\ (x, xs) -> map ((x ++ ":") ++) xs) ag11
    -- exact name matches are prefered

    ag11
      | null ag10 = filterNotNull $
                    map (second (filter (n `isPrefixOf`))) ag01
      | otherwise = ag10

    ag10 = filterNotNull $
      map (second (filter (== n))) ag01

    -- exact group matches are prefered
    ag01
      | null ag00 = filter ((g `isPrefixOf`) . fst) attrGroups
      | otherwise = ag00

    ag00
      | null g    = attrGroups
      | otherwise = filter ((== g) .fst) attrGroups

-- ----------------------------------------

attrGroups :: [AttrGroup]
attrGroups =
  [ attrFile
  , attrExif
  , attrComposite
  , attrXmp
  ]

attrFile :: AttrGroup
attrFile =
  ( "File"
  , [ "FileName"
    , "Directory"
    , "FileSize"
    , "FileModifyDate"
    , "MIMEType"
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


attrComposite :: AttrGroup
attrComposite =
  ( "Composite"
  , [ "Aperture"
    , "AutoFocus"
    , "CircleOfConfusion"
    , "DOF"
    , "Flash"
    , "FocalLength35efl"
    , "FocalLength35efl"
    , "FOV"
    , "GPSLatitudeRef"
    , "GPSLongitudeRef"
    , "GPSPosition"
    , "HyperfocalDistance"
    , "ImageSize"
    , "LensID"
    , "LensSpec"
    , "LightValue"
    , "Megapixels"
    , "ShutterSpeed"
    , "SubSecDateTimeOriginal"
    ]
  )

attrXmp :: AttrGroup
attrXmp =
  ("XMP"
  , [ "GPSLatitude"
    , "GPSLongitude"
    , "Format"
    , "RawFileName"
    ]
  )

-- ----------------------------------------
