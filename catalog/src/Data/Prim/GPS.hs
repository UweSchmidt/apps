{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.GPS
  ( GPSposDeg
  , GPSposDec
  , GPSdeg
  , gpsLat
  , gpsLong
  , isoDegDec
  , isoGoogleMapsDegree
  , googleMapsGPSdec
  )
where

import           Data.Prim.Prelude
import           Text.SimpleParser
import           Text.Printf         ( printf )

-- ----------------------------------------

data GPSdir = N | E | S | W deriving (Eq, Show)

data GPSdeg = GPSdeg !Int !Int !Double !GPSdir deriving (Eq, Show)

data GPSpos' a =
  GPSpos { _gpsLat  :: !a
         , _gpsLong :: !a
         }
  deriving Show

type GPSposDeg = GPSpos' GPSdeg  -- GPS position (lat, long) in deg, min, sec, dir
type GPSposDec = GPSpos' Double  -- GPS position (lat, long) in decimal degrees


gpsLat :: Lens' (GPSpos' a) a
gpsLat k gp = (\ new -> gp {_gpsLat = new}) <$> k (_gpsLat gp)

gpsLong :: Lens' (GPSpos' a) a
gpsLong k gp = (\ new -> gp {_gpsLong = new}) <$> k (_gpsLong gp)

-- conversion of degree from/to decimal degree
-- and parse/show can be implemeted by an iso and two prisms

-- conversion decimal <-> degree

isoDegDec :: Iso' GPSposDeg GPSposDec
isoDegDec = iso toDec frDec
  where
    toDec (GPSpos lat long) = GPSpos (deg2dec       lat) (deg2dec       long)
    frDec (GPSpos lat long) = GPSpos (dec2deg (N,S) lat) (dec2deg (E,W) long)

-- parse and show positions in degree format

instance StringPrism GPSposDeg where
  stringPrism = prism' showPos
                       (parseMaybe parserPos)
    where
      showPos :: GPSposDeg -> String
      showPos (GPSpos lat long) =
        showDeg lat
        ++ ", " ++
        showDeg long

      parserPos :: SP GPSposDeg
      parserPos = do
        lat  <- msp *> parserDeg [N, S] <* msp <* char ','
        long <- msp *> parserDeg [E, W] <* msp
        return $ GPSpos lat long

-- parse and show positions in decimal form
--
-- "53.3, 10.0" ^? stringPrism      -> Just (GPSpos 53.3 10.0)
-- stringPrism # (GPSpos 53.3 10.0) -> "53.3, 10.0"

instance StringPrism GPSposDec where
  stringPrism = prism' showPosDec
                       (parseMaybe parserPos)
    where
      showPosDec :: GPSposDec -> String
      showPosDec (GPSpos lat long) =
        printf "%.9f" lat
        ++ "," ++
        printf "%.9f" long

      parserPos :: SP GPSposDec
      parserPos = msp *> parserPosDec <* msp

-- the googleMaps parser accepts various input formats
-- as GPS position
--
-- .1 a google maps url with a part .../@53.0,10.0,17z...
-- .2 a pair of decimal lat/long coords: 53.0,10.0
-- .3 the GPS format in EXIF data
--
-- these formats are parsed and the result is maybe a pair of doubles

googleMapsGPSdec :: Prism' String GPSposDec
googleMapsGPSdec = prism' pos2mapsUrl
                         (\ s -> parseMaybe parserMapsUrl s
                                 <|>
                                 (s ^? stringPrism . isoDegDec)
                         )
  where
    pos2mapsUrl :: GPSposDec -> String
    pos2mapsUrl pos =
      "https://maps.google.de/maps/@" <> (stringPrism # pos) <> ",17z"

    -- parse a Google maps url or just a GPSposDec (pair of doubles)
    parserMapsUrl :: SP GPSposDec
    parserMapsUrl =
      try ( anyStringThen "/@" *> parserPosDec <*
            char ',' <* some digitChar <* char 'z' <* many anyChar
          )
      <|>
      try parserPosDec

-- a use case for the whole GPS stuff:
--
-- transform a Google Maps url into a string representing and EXIF GPS position
-- and vice versa

isoGoogleMapsDegree :: Iso' String String
isoGoogleMapsDegree =
  iso googleMapsUrl2degree degree2googleMapsUrl
  where
    googleMapsUrl2degree :: String -> String
    googleMapsUrl2degree url =
      res ^. from isoMaybe
      where
        deg :: Maybe GPSposDeg
        deg = url ^? googleMapsGPSdec . from isoDegDec

        res :: Maybe String
        res = (stringPrism #) <$> deg

    degree2googleMapsUrl :: String -> String
    degree2googleMapsUrl deg =
      res ^. from isoMaybe
      where
        dec :: Maybe GPSposDec
        dec = deg ^? stringPrism . isoDegDec

        res :: Maybe String
        res = (stringPrism #) <$> dec

instance StringPrism GPSdeg where
  stringPrism = prism' showDeg
                       (parseMaybe $ parserDeg [N, E, S, W])

-- helper funtions

parserPosDec :: SP GPSposDec
parserPosDec = tt <$> signedFloat <* msp <* char ',' <* msp <*> signedFloat
  where
    tt x y = GPSpos (read x) (read y)

showDeg :: GPSdeg -> String
showDeg (GPSdeg d m s r) = unwords
  [ show d, "deg", show m ++ "'", printf "%.9f" s ++ "\"", show r]

parserDeg :: [GPSdir] -> SP GPSdeg
parserDeg dirs = do
  deg <- read <$> (some digitChar <* msp <* string "deg" <* msp)
  mn  <- read <$> (some digitChar <* char '\''           <* msp)
  sec <- read <$> (floatParser    <* char '"'            <* msp)
  dir <- parserDir
  return $ GPSdeg deg mn sec dir
  where
    parserDir :: SP GPSdir
    parserDir = foldr (<|>) empty $ map parserD dirs
      where
        parserD :: GPSdir -> SP GPSdir
        parserD d = string (show d) *> return d

deg2dec :: GPSdeg -> Double
deg2dec (GPSdeg d m s r) =
  (d' + m'/60 + s/3600) *
  ( if r == N || r == E
    then  1
    else -1
  )
  where
    d' = fromIntegral d
    m' = fromIntegral m

dec2deg :: (GPSdir, GPSdir) -> Double -> GPSdeg
dec2deg (po, ne) x = GPSdeg d'  m'  s'  c'
  where
    s'       = r2 * 60
    (m', r2) = properFraction (r1 * 60)
    (d', r1) = properFraction x'
    (x', c')
      | x >= 0    = ( x, po)
      | otherwise = (-x, ne)

signedFloat :: SP String
signedFloat =
  (string "-" <|> string "+" <|> return "")
  <++>
  floatParser

floatParser :: SP String
floatParser =
  ( some digitChar
    <++>
    option ".0"
    ( string "."
      <++>
      ( option "0" $ some digitChar )
    )
  )
  <|>
  (("0." ++) <$> (char '.' *> some digitChar))

-- ----------------------------------------
--
-- Test data

{-
s1, s2, s3 :: String

s1 = " 53.575644, -9.767767 "
s2 = "https://www.google.com/maps/place/34%C2%B011'19.1%22N+118%C2%B040'26.5%22W/@34.1886344,-118.6762237,17z/data=!3m1!4b1!4m14!1m7!3m6!1s0x80c2c75ddc27da13:0xe22fdf6f254608f4!2sLos+Angeles,+CA,+USA!3b1!8m2!3d34.0522342!4d-118.2436849!3m5!1s0x0:0x0!7e2!8m2!3d34.1886303!4d-118.6740354"
s3 = "53 deg 2' 10.3\" N , 10 deg 0' 20.1\" E"

-- -}
