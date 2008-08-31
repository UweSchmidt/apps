module Photo2.ExifData
where

import           Control.Arrow
import		 Control.Monad ( mplus )

import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes

import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch
import           Text.XML.HXT.DOM.Util ( stringTrim )

parseExif	:: String -> Attrs
parseExif
    = lines
      >>>
      map (splitKeyVal ':')
      >>>
      map ( (words >>> concatMap capitalize >>> newAttrKey)
	    ***
	    (words >>> unwords >>> normDateTime)
	  )
      >>>
      filter ( not . null . snd )
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
    = fromMaybe False . matchRE r
    where
    r = d ++ " " ++ t ++ s
    d = "[0-9]{4}:[0-9]{2}:[0-9]{2}"
    t = "[0-9]{2}:[0-9]{2}:[0-9]{2}"
    s = "([.][0-9]{2})?"

normAttrs	:: Attrs -> Attrs
normAttrs
    = M.foldWithKey norm emptyAttrs
    where
    norm k v m
	| null k'	= m
	| otherwise	= M.insert k' v m
	where
	k' = newAttrKey k

newAttrKey	:: String -> String
newAttrKey k
    = addPrefix $
      fromMaybe ""
      ( lookup k oldKeys
	`mplus`
	lookup k' (map ((\ x -> (x,x)) . snd) oldKeys)
	`mplus`
	return k
      )
    where
    k' = filter (/= '-') k

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
      , ("manufacturer", "Make")
      , ("metering-mode", "MeteringMode")
      , ("model", "CameraModelName")
      , ("quality", "Quality")
      , ("ref-org", "RefOrig")
      , ("ref-raw", "RefRaw")
      , ("ref-xmp", "RefXmp")
      , ("saturation", "Saturation")
      , ("sharpness", "Sharpness")
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
      , ( "Make",			"cam"   )
      , ( "MeteringMode",		"exif"  )
      , ( "Quality",			"exif"  )
      , ( "RefOrig",			"file"  )
      , ( "RefRaw",			"file"  )
      , ( "RefXmp",			"file"  )
      , ( "Saturation",			"exif"  )
      , ( "Sharpness",			"exif"  )
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
