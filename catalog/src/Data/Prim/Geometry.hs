{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.Geometry
where

import Data.Prim.Prelude
import Text.SimpleParser

-- ----------------------------------------

data Geo = Geo !Int !Int

deriving instance Show Geo
deriving instance Eq Geo
deriving instance Ord Geo

instance FromJSON Geo where
  parseJSON v = do
    Just geo <- readGeo' <$> parseJSON v
    return geo

instance ToJSON   Geo where
  toJSON geo = toJSON (geo ^. isoText)

instance IsoString Geo where
  isoString = iso showGeo readGeo
  {-# INLINE isoString #-}

instance IsoText Geo where
  isoText = isoString . isoText
  {-# INLINE isoText #-}

instance Monoid Geo where
  mempty = Geo 0 0
  Geo 0 0 `mappend` geo2 = geo2
  geo1    `mappend` _    = geo1

instance IsEmpty Geo where
  isempty g = g == mempty

geo'org :: Geo
geo'org = Geo 1 1

orgGeo :: String
orgGeo = "org"

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
readGeo' = parseMaybe geoParser

geoParser :: SP Geo
geoParser = try pg <|> pg'org
  where
    pg'org = string "org" >> return geo'org

    pg = do
      x <- read <$> some digitChar
      _ <- char 'x'
      y <- read <$> some digitChar
      return (Geo x y)

flipGeo :: Geo -> Geo
flipGeo (Geo w h) = Geo h w

-- ----------------------------------------

data AspectRatio = Fix | Pad | Crop
                 deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance IsoString AspectRatio where
  isoString = iso toS frS
    where
      toS = (\ (x : xs) -> toLower x : xs) . show
      frS = read . (\ (x : xs) -> toUpper x : xs)

instance IsoText AspectRatio where
  isoText = isoString . isoText

arParser :: SP AspectRatio
arParser =
  foldr1 (<|>) $ map toAP [minBound .. maxBound]
  where
    toAP :: AspectRatio -> SP AspectRatio
    toAP ar = try (string (ar ^. isoString) >> return ar)

-- ----------------------------------------

data GeoAR = GeoAR !Int !Int !AspectRatio

deriving instance Eq   GeoAR
deriving instance Ord  GeoAR
deriving instance Show GeoAR

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

instance FromJSON GeoAR where
  parseJSON v = do
    Just geo <- readGeoAR <$> parseJSON v
    return geo

instance ToJSON GeoAR where
  toJSON geo = toJSON (geo ^. isoText)


mkGeoAR :: Geo -> AspectRatio -> GeoAR
mkGeoAR (Geo w h) = GeoAR w h

geoar'org :: GeoAR
geoar'org = (geo'org, Pad) ^. from geoar2pair

geoar2pair :: Iso' GeoAR (Geo, AspectRatio)
geoar2pair = iso (\ (GeoAR w h ar) -> (Geo w h, ar))
                 (\ (Geo w h, ar) -> GeoAR w h ar)

theGeo :: Lens' GeoAR Geo
theGeo = geoar2pair . _1

theAR :: Lens' GeoAR AspectRatio
theAR  = geoar2pair . _2

readGeoAR :: String -> Maybe GeoAR
readGeoAR = parseMaybe geoARParser

geoARParser :: SP GeoAR
geoARParser = do
  ar  <- arParser <* char '-'
  geo <- geoParser
  return $ (geo, ar) ^. from geoar2pair

isoGeoAR :: Iso' String (Maybe GeoAR)
isoGeoAR = iso readGeoAR (maybe "" (^. isoString))

-- ----------------------------------------
