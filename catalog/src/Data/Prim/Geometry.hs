{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.Geometry
where

import Data.Prim.Prelude

-- ----------------------------------------

data Geo = Geo !Int !Int

deriving instance Show Geo
deriving instance Eq Geo
deriving instance Ord Geo

instance IsoString Geo where
  isoString = iso showGeo readGeo
  {-# INLINE isoString #-}

instance IsoText Geo where
  isoText = isoString . isoText
  {-# INLINE isoText #-}

geo2pair :: Iso' Geo (Int, Int)
geo2pair = iso (\ (Geo w h) -> (w, h)) (uncurry Geo)

theW :: Lens' Geo Int
theW = geo2pair . _1

theH :: Lens' Geo Int
theH = geo2pair . _2

showGeo :: Geo -> String
showGeo (Geo w h) = show w ++ "x" ++ show h

readGeo :: String -> Geo
readGeo s =
  fromMaybe (error $ "parseGeo: no parse: " ++ s) $
  readGeo' s

readGeo' :: String -> Maybe Geo
readGeo' s =
  build $
  matchSubexRE geoRegex s
  where
    build [("w",w),("h",h)]
      = Just $ Geo (read w) (read h)
    build _
      = Nothing

flipGeo :: Geo -> Geo
flipGeo (Geo w h) = Geo h w

-- the r.e. is a more general, the geo spec maybe surrounded
-- by arbitrary other stuff (used in parsing exit data)

geoRegex :: Regex
geoRegex = parseRegexExt geoRegex'

geoRegex'
  , geoRegex'' :: String

geoRegex'  = "[^1-9]*({w}[1-9][0-9]*)x({h}[1-9][0-9]*)[^0-9]*"
geoRegex'' =        "({w}[1-9][0-9]*)x({h}[1-9][0-9]*)"

-- ----------------------------------------

data AspectRatio = Fix | Pad | Crop | Pano
                 deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance IsoString AspectRatio where
  isoString = iso toS frS
    where
      toS = (\ (x : xs) -> toLower x : xs) . show
      frS = read . (\ (x : xs) -> toUpper x : xs)

instance IsoText AspectRatio where
  isoText = isoString . isoText

arRegex :: Regex
arRegex = parseRegexExt arRegex'

arRegex' :: String
arRegex' =
  intercalate "|" $
  map (^. isoString) [minBound .. maxBound :: AspectRatio]

-- ----------------------------------------

data GeoAR = GeoAR !Int !Int !AspectRatio

deriving instance Show GeoAR

geoar2pair :: Iso' GeoAR (Geo, AspectRatio)
geoar2pair = iso (\ (GeoAR w h ar) -> (Geo w h, ar))
                 (\ (Geo w h, ar) -> GeoAR w h ar)

theGeo :: Lens' GeoAR Geo
theGeo = geoar2pair . _1

theAR :: Lens' GeoAR AspectRatio
theAR  = geoar2pair . _2

instance IsoString GeoAR where
  isoString = iso toS frS
    where
      toS x = (x ^. geoar2pair . _2 . isoString) ++ "-" ++
              (x ^. geoar2pair . _1 . isoString)

      frS s =  ( g  ^. from isoString
               , ar ^. from isoString
               ) ^. from geoar2pair
        where
          (ar, '-' : g) = break (== '-') s

instance IsoText GeoAR where
  isoText = isoString . isoText

geoarRegex :: Regex
geoarRegex = parseRegexExt geoarRegex'

geoarRegex' :: String
geoarRegex' = "({ar}" ++ arRegex' ++ ")-" ++ geoRegex''

readGeoAR :: String -> Maybe GeoAR
readGeoAR = build . matchSubexRE geoarRegex
  where
    build [("ar", ar), ("w", w), ("h", h)] =
      Just $ GeoAR (read w) (read h) (ar ^. from isoString)
    build _ =
      Nothing

isoGeoAR :: Iso' String (Maybe GeoAR)
isoGeoAR = iso readGeoAR (maybe "" (^. isoString))

-- ----------------------------------------
