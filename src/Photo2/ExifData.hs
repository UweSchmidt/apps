module Photo2.ExifData
where

import           Control.Arrow

import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes

import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

parseExif	:: String -> Attrs
parseExif
    = lines
      >>>
      map ( break (== ':')
	    >>>
	    ( ( map toAlphaNum >>> words >>> intercalate "-" )
	      ***
	      ( drop 1 >>> words >>> unwords >>> normDateTime )
	    )
	  )
      >>>
      filter ( not . null . snd )
      >>>
      filter ( isAttrKey . fst )
      >>>
      M.fromList

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
    = fromMaybe "" . lookup k $
      [ ("modified", "File-Modification-Date-Time")
      , ("geometry", "Image-Size")
      , ("color-space", "Color-Space-Data")
      , ("comment", "Comment")
      , ("contrast", "Contrast")
      , ("date-and-time", "Create-Date")
      , ("date-and-time-original", "Date-Time-Original")
      , ("exposure-compensation", "Exposure-Compensation")
      , ("exposure-mode", "Exposure-Mode")
      , ("exposure-program", "Exposure-Program")
      , ("exposure-time", "Exposure-Time")
      , ("file-size", "File-Size")
      , ("flash", "Flash")
      , ("fnumber", "F-Number")
      , ("focal-length", "Focal-Length")
      , ("focal-length-in-35mm", "Focal-Length-In-35mm-Format")
      , ("focus-mode", "Focus-Mode")
      , ("iso-setting", "ISO")
      , ("keywords", "Keywords")
      , ("lens", "Lens")
      , ("lens-id", "Lens-ID")
      , ("manufacturer", "Make")
      , ("metering-mode", "Metering-Mode")
      , ("model", "Camera-Model-Name")
      , ("quality", "Quality")
      , ("ref-org", "Ref-Orig")
      , ("ref-raw", "Ref-Raw")
      , ("ref-xmp", "Ref-Xmp")
      , ("saturation", "Saturation")
      , ("sharpness", "Sharpness")
      , ("subject", "Subject")
      , ("subtitle", "Subtitle")
      , ("title", "Title")
      , ("user-comment", "User-Comment")
      , ("white-balance", "White-Balance")
      ]

isAttrKey	:: String -> Bool
isAttrKey
    = (`elem` [ "Aperture"
	      , "Camera-Model-Name"
	      , "Color-Space-Data"
	      , "Comment"
	      , "Contrast"
	      , "Create-Date"
	      , "Date-Time-Original"
	      , "Exposure-Compensation"
	      , "Exposure-Mode"
	      , "Exposure-Program"
	      , "Exposure-Time"
	      , "F-Number"
	      , "File-Modification-Date-Time"
	      , "File-Size"
	      , "Flash"
	      , "Focal-Length"
	      , "Focal-Length-In-35mm-Format"
	      , "Focus-Distance"
	      , "Focus-Mode"
	      , "ISO"
	      , "Image-Size"
	      , "Keywords"
	      , "Lens"
	      , "Lens-ID"
	      , "Make"
	      , "Metering-Mode"
	      , "Quality"
	      , "Ref-Orig"
	      , "Ref-Raw"
	      , "Ref-Xmp"
	      , "Saturation"
	      , "Sharpness"
	      , "Shutter-Speed"
	      , "Subject"
	      , "Subtitle"
	      , "Title"
	      , "User-Comment"
	      , "Vibration-Reduction"
	      , "White-Balance"
	      ]
      )
