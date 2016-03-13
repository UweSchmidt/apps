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
  isoText = isoString . from isoString
  {-# INLINE isoText #-}

geo2pair :: Iso' Geo (Int, Int)
geo2pair = iso (\ (Geo w h) -> (w, h)) (uncurry Geo)

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

-- the r.e. is a more general, the geo spec maybe surrounded
-- by arbitrary other stuff (used in parsing exit data)

geoRegex :: Regex
geoRegex =
  parseRegexExt "[^1-9]*({w}[1-9][0-9]*)x({h}[1-9][0-9]*)[^0-9]*"
  --parseRegexExt "({w}[1-9][0-9]*)x({h}[1-9][0-9]*)"

-- ----------------------------------------

data AspectRatio = Fix | Pad | Crop
                 deriving (Eq, Show, Read)

instance IsoString AspectRatio where
  isoString = iso toS frS
    where
      toS = (\ (x : xs) -> toLower x : xs) . show
      frS = read . (\ (x : xs) -> toUpper x : xs)

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

-- ----------------------------------------
