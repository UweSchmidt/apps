module Photo2.ExifData
where

import           Control.Arrow
import           Control.Monad ( ) -- mplus )

import           Data.Atom
import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes

import           Text.Regex.XMLSchema.Generic
import           Text.XML.HXT.DOM.Util ( stringTrim )

-- ------------------------------------------------------------

titleKey, subTitleKey, resourceKey,
  googleMapsKey, webKey, wikipediaKey,
  durationKey,
  gpsLocationKey, gpsLatitudeKey, gpsLongitudeKey,
  fileModificationKey :: Atom

titleKey                        = newAtom "descr:Title"
subTitleKey                     = newAtom "descr:Subtitle"
resourceKey                     = newAtom "descr:Resource"
googleMapsKey                   = newAtom "descr:GoogleMaps"
webKey                          = newAtom "descr:Web"
wikipediaKey                    = newAtom "descr:Wikipedia"
durationKey                     = newAtom "show:Duration"
gpsLocationKey                  = newAtom "geo:GPSLocation"
gpsLatitudeKey                  = newAtom gpsLatitude
gpsLongitudeKey                 = newAtom gpsLongitude
fileModificationKey             = newAtom fileModificationDateTime

gpsLatitude', gpsLongitude', fileModificationDateTime'       :: String
gpsLatitude'                    = "GPSLatitude"
gpsLongitude'                   = "GPSLongitude"
fileModificationDateTime'       = "FileModificationDateTime"


gpsLatitude, gpsLongitude, fileModificationDateTime        :: String
gpsLatitude                     = addPrefix gpsLatitude'
gpsLongitude                    = addPrefix gpsLongitude'
fileModificationDateTime        = addPrefix fileModificationDateTime'

-- ------------------------------------------------------------

parseExif       :: PicAttrs -> String -> Attrs
parseExif pl
    = lines
      >>>
      map (splitKeyVal ':')
      >>>
      map ( ( map (\ c -> if isAlphaNum c then c else ' ')
              >>>
              words
              >>>
              concatMap capitalize
              >>>
              newAttrKey pl
            )
            ***
            ( words >>> unwords >>> normDateTime )
          )
      >>>
      filter ( not . null . snd )
      >>>
      filter ( not . ("unknown:" `isPrefixOf`) . show . fst )
      >>>
      M.fromList

splitKeyVal     :: Char -> String -> (String, String)
splitKeyVal c
    = break (== c)
      >>>
      ( stringTrim
        ***
        ( drop 1 >>> stringTrim )
      )

splitWith       :: Char -> String -> (String, String)
splitWith c
    = splitKeyVal c
      >>>
      ( \ (x,y) -> if null y then (y,x) else (x,y) )

capitalize      :: String -> String
capitalize (c:s1)       | isAlpha c     = toUpper c : s1
capitalize s                            = s

toAlphaNum      :: Char -> Char
toAlphaNum c
    | isAlphaNum c      = c
    | otherwise         = ' '

normDateTime    :: String -> String
normDateTime s
    | isDateTime s      = d' ++ t
    | otherwise         = s
    where
    (d, t) = break (== ' ') s
    d'     = map (\ c -> if c == ':' then '-' else c) d

isDateTime      :: String -> Bool
isDateTime
    = match r
    where
    r = d ++ " " ++ t ++ s ++ z
    d = "[0-9]{4}:[0-9]{2}:[0-9]{2}"
    t = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
    s = "([.][0-9]{2})?"
    z = "([-+][0-9]{2}:[0-9]{2}([.][0-9]{2})?)?"

normAttrs       :: PicAttrs -> Attrs -> Attrs
normAttrs pl
    = M.foldrWithKey norm emptyAttrs
    where
    norm k v m
        | null . show $ k'      = m
        | otherwise             = M.insert k' v m
        where
        k' = newAttrKey pl (show k)

newAttrKey      :: PicAttrs -> String -> Atom
newAttrKey pl k
    = newAtom $
      case matchFound pl of
      Just n    -> n
      Nothing   -> ("unknown:" ++) . snd . splitWith ':' $ k
    where
    matchKey    :: Value -> Name -> Maybe Name
    matchKey p n
        | match p k = Just n
        | otherwise = Nothing

    foldMatch :: Maybe Name -> (Value, Name) -> Maybe Name
    foldMatch r@(Just _) _ = r
    foldMatch Nothing    e = uncurry matchKey e

    matchFound :: PicAttrs -> Maybe Name
    matchFound = foldl foldMatch Nothing

-- | "degrees minutes seconds" to "decimal degrees" (google url format)
degToDecDeg :: String -> String
degToDecDeg loc
  = case matchSubex pattern loc of
     [("deg", deg), ("min", mn), ("sec", sec), ("dir", dir)]
       -> show $ conv (read deg) (read mn) (read sec) (head dir)
     _ -> ""
    where
      pattern
        = "({deg}[0-9]+) +deg +({min}[0-9]+)' +({sec}[.0-9]+)\" +({dir}[NWES])"

      conv :: Double -> Double -> Double -> Char -> Double
      conv d m s dir
        = (d + m/60 + s/3600) * (if dir `elem` "WS" then (0-1) else 1)

locationToGoogleMaps :: String -> String
locationToGoogleMaps loc
  = case matchSubex pattern loc of
     [("lat", lat), ("long", long)]
       -> toGoogleMaps lat long
     _ -> ""
  where
    pattern
      = "({lat}[^NS]+[NS]) +({long}[^WE]+[WE])"

-- | build a google maps url from latitude and longitude
toGoogleMaps :: String -> String -> String
toGoogleMaps lat long
  | null lat' || null long'
    = ""
  | otherwise
    = "https://maps.google.com/maps?ll=" ++ lat' ++ "," ++ long' ++ "&amp;z=" ++ zoom
  where
    zoom  = "17"
    lat'  = degToDecDeg $ lat
    long' = degToDecDeg $ long
    
-- ------------------------------------------------------------

oldKeys :: [(String, String)]
oldKeys
    = [ ("modified", fileModificationDateTime')
      , ("duration", "Duration")
      , ("geometry", "ImageSize")
      , ("color-space", "ColorSpaceData")
      , ("comment", "Comment")
      , ("contrast", "Contrast")
      , ("date-and-time", "CreateDate")
      , ("date-and-time-original", "DateTimeOriginal")
      , ("exposure-bias", "ExposureCompensation")
      , ("exposure-compensation", "ExposureCompensation")
      , ("exposure-mode", "ExposureMode")
      , ("exposure-program", "ExposureProgram")
      , ("exposure-time", "ExposureTime")
      , ("file-size", "FileSize")
      , ("flash", "Flash")
      , ("fnumber", "FNumber")
      , ("focal-length", "FocalLength")
      , ("focal-length-in-35mm", "FocalLengthIn35mmFormat")
      , ("focus-distance", "FocusDistance")
      , ("focus-mode", "FocusMode")
      , ("iso-setting", "ISO")
      , ("keywords", "Keywords")
      , ("lens", "Lens")
      , ("lens-id", "LensID")
      , ("lens-spec", "LensSpec")
      , ("manufacturer", "Make")
      , ("metering-mode", "MeteringMode")
      , ("model", "CameraModelName")
      , ("quality", "Quality")
      , ("ref-org", "RefOrig")
      , ("ref-raw", "RefRaw")
      , ("ref-xmp", "RefXmp")
      , ("resource", "Resource")
      , ("saturation", "Saturation")
      , ("sharpness", "Sharpness")
      , ("shooting-mode", "ShootingMode")
      , ("shutter-speed", "ShutterSpeed")
      , ("subject", "Subject")
      , ("subtitle", "Subtitle")
      , ("title", "Title")
      , ("user-comment", "UserComment")
      , ("vibration-reduction", "VibrationReduction")
      , ("white-balance", "WhiteBalance")
      ]

addPrefix       :: String -> String
addPrefix s
    | null v    = (fromMaybe "unknown" . lookup k $ prefixMap) ++ ":" ++ k
    | otherwise = s
    where
    (k, v) = splitKeyVal ':' s

prefixMap       :: [(String, String)]
prefixMap
    = [ ( "Aperture",                   "exif"  )
      , ( "CameraModelName",            "cam"   )
      , ( "ColorSpaceData",             "exif"  )
      , ( "Comment",                    "descr" )
      , ( "Contrast",                   "exif"  )
      , ( "CreateDate",                 "exif"  )
      , ( "DateTimeOriginal",           "exif"  )
      , ( "DepthOfField",               "exif"  )
      , ( "Duration",                   "show"  )
      , ( "ExposureCompensation",       "exif"  )
      , ( "ExposureMode",               "exif"  )
      , ( "ExposureProgram",            "exif"  )
      , ( "ExposureTime",               "exif"  )
      , ( "FNumber",                    "exif"  )
      , ( fileModificationDateTime',    "file"  )
      , ( "FileSize",                   "file"  )
      , ( "Flash",                      "exif"  )
      , ( "FocalLength",                "cam"   )
      , ( "FocalLengthIn35mmFormat",    "cam"   )
      , ( "FocusDistance",              "exif"  )
      , ( "FocusMode",                  "exif"  )
      , ( "ISO",                        "exif"  )
      , ( "ImageSize",                  "exif"  )
      , ( "Keywords",                   "descr" )
      , ( "Lens",                       "cam"   )
      , ( "LensID",                     "cam"   )
      , ( "LensSpec",                   "cam"   )
      , ( "Make",                       "cam"   )
      , ( "MeteringMode",               "exif"  )
      , ( "Quality",                    "exif"  )
      , ( "RefOrig",                    "file"  )
      , ( "RefRaw",                     "file"  )
      , ( "RefXmp",                     "file"  )
      , ( "Resource",                   "descr" )
      , ( "Saturation",                 "exif"  )
      , ( "Sharpness",                  "exif"  )
      , ( "ShootingMode",               "exif"  )
      , ( "ShutterSpeed",               "exif"  )
      , ( "Subject",                    "descr" )
      , ( "Subtitle",                   "descr" )
      , ( "Title",                      "descr" )
      , ( "UserComment",                "descr" )
      , ( "VibrationReduction",         "exif"  )
      , ( "WhiteBalance",               "exif"  )
      , ( gpsLatitude',                 "geo"   )
      , ( "GPSLatitudeRef",             "geo"   )
      , ( gpsLongitude',                "geo"   )
      , ( "GPSLongitudeRef",            "geo"   )
      , ( "GPSAltitude",                "geo"   )
      , ( "GPSAltitudeRef",             "geo"   )
      , ( "GPSDateStamp",               "geo"   )
      , ( "GPSTimeStamp",               "geo"   )
      ]

isAttrKey       :: String -> Bool
isAttrKey
    = (`elem` [ "Aperture"
              , "CameraModelName"
              , "ColorSpaceData"
              , "Comment"
              , "Contrast"
              , "CreateDate"
              , "DateTimeOriginal"
              , "DepthOfField"
              , "ExposureCompensation"
              , "ExposureMode"
              , "ExposureProgram"
              , "ExposureTime"
              , "FNumber"
              , fileModificationDateTime'
              , "FileSize"
              , "Flash"
              , "FocalLength"
              , "FocalLengthIn35mmFormat"
              , "FocusDistance"
              , "FocusMode"
              , gpsLatitude'
              , gpsLongitude'
              , "GPSAltitude"
              , "GPSDateStamp"
              , "GPSTimeStamp"
              , "ISO"
              , "ImageSize"
              , "Keywords"
              , "Lens"
              , "LensID"
              , "Make"
              , "MeteringMode"
              , "Quality"
              , "RefOrig"
              , "RefRaw"
              , "RefXmp"
              , "Saturation"
              , "Sharpness"
              , "ShutterSpeed"
              , "Subject"
              , "Subtitle"
              , "Title"
              , "UserComment"
              , "VibrationReduction"
              , "WhiteBalance"
              ]
      )
      
-- ------------------------------------------------------------
