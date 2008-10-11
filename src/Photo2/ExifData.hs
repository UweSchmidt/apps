module Photo2.ExifData
where

import           Control.Arrow
import		 Control.Monad ( ) -- mplus )

import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes

import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
import           Text.XML.HXT.DOM.Util ( stringTrim )

parseExif	:: PicAttrs -> String -> Attrs
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
      filter ( not . ("unknown:" `isPrefixOf`) . fst )
      >>>
      M.fromList

splitKeyVal	:: Char -> String -> (String, String)
splitKeyVal c
    = break (== c)
      >>>
      ( stringTrim
	***
	( drop 1 >>> stringTrim )
      )

splitWith	:: Char -> String -> (String, String)
splitWith c
    = splitKeyVal c
      >>>
      ( \ (x,y) -> if null y then (y,x) else (x,y) )

capitalize	:: String -> String
capitalize (c:s1)	| isAlpha c	= toUpper c : s1
capitalize s				= s

toAlphaNum	:: Char -> Char
toAlphaNum c
    | isAlphaNum c	= c
    | otherwise		= ' '

normDateTime	:: String -> String
normDateTime s
    | isDateTime s	= d' ++ t
    | otherwise		= s
    where
    (d, t) = break (== ' ') s
    d'     = map (\ c -> if c == ':' then '-' else c) d

isDateTime	:: String -> Bool
isDateTime
    = match r
    where
    r = d ++ " " ++ t ++ s
    d = "[0-9]{4}:[0-9]{2}:[0-9]{2}"
    t = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
    s = "([.][0-9]{2})?"

normAttrs	:: PicAttrs -> Attrs -> Attrs
normAttrs pl
    = M.foldWithKey norm emptyAttrs
    where
    norm k v m
	| null k'	= m
	| otherwise	= M.insert k' v m
	where
	k' = newAttrKey pl k

newAttrKey	:: PicAttrs -> String -> String
newAttrKey pl k
    = case matchFound pl of
      Just n	-> n
      Nothing   -> ("unknown:" ++) . snd . splitWith ':' $ k
    where
    matchKey	:: Value -> Name -> Maybe Name
    matchKey p n
	| match p k = Just n
	| otherwise = Nothing

    foldMatch :: Maybe Name -> (Value, Name) -> Maybe Name
    foldMatch r@(Just _) _ = r
    foldMatch Nothing	 e = uncurry matchKey e

    matchFound :: PicAttrs -> Maybe Name
    matchFound = foldl foldMatch Nothing

fileModificationDateTime'	:: String
fileModificationDateTime'	= "FileModificationDateTime"

fileModificationDateTime	:: String
fileModificationDateTime	= addPrefix fileModificationDateTime'

oldKeys	:: [(String, String)]
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

addPrefix	:: String -> String
addPrefix s
    | null v	= (fromMaybe "unknown" . lookup k $ prefixMap) ++ ":" ++ k
    | otherwise = s
    where
    (k, v) = splitKeyVal ':' s

prefixMap	:: [(String, String)]
prefixMap
    = [ ( "Aperture",			"exif"  )
      , ( "CameraModelName",		"cam"   )
      , ( "ColorSpaceData",		"exif"  )
      , ( "Comment",			"descr" )
      , ( "Contrast",			"exif"  )
      , ( "CreateDate",			"exif"  )
      , ( "DateTimeOriginal",		"exif"  )
      , ( "DepthOfField",		"exif"  )
      , ( "Duration",			"show"  )
      , ( "ExposureCompensation",	"exif"  )
      , ( "ExposureMode",		"exif"  )
      , ( "ExposureProgram",		"exif"  )
      , ( "ExposureTime",		"exif"  )
      , ( "FNumber",			"exif"  )
      , ( fileModificationDateTime',	"file"  )
      , ( "FileSize",			"file"  )
      , ( "Flash",			"exif"  )
      , ( "FocalLength",		"cam"   )
      , ( "FocalLengthIn35mmFormat",	"cam"   )
      , ( "FocusDistance",		"exif"  )
      , ( "FocusMode",			"exif"  )
      , ( "ISO",			"exif"  )
      , ( "ImageSize",			"exif"  )
      , ( "Keywords",			"descr" )
      , ( "Lens",			"cam"   )
      , ( "LensID",			"cam"   )
      , ( "LensSpec",			"cam"   )
      , ( "Make",			"cam"   )
      , ( "MeteringMode",		"exif"  )
      , ( "Quality",			"exif"  )
      , ( "RefOrig",			"file"  )
      , ( "RefRaw",			"file"  )
      , ( "RefXmp",			"file"  )
      , ( "Resource",			"descr" )
      , ( "Saturation",			"exif"  )
      , ( "Sharpness",			"exif"  )
      , ( "ShootingMode",		"exif"  )
      , ( "ShutterSpeed",		"exif"  )
      , ( "Subject",			"descr" )
      , ( "Subtitle",			"descr" )
      , ( "Title",			"descr" )
      , ( "UserComment",		"descr" )
      , ( "VibrationReduction",		"exif"  )
      , ( "WhiteBalance",		"exif"  )
      ]

isAttrKey	:: String -> Bool
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