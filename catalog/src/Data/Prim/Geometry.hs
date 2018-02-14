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
readGeo' s =
  build $ matchSubexRE geoRegex s
  where
    build [("w",w),("h",h)]
      = Just $ Geo (read w) (read h)
    build _
      | s == orgGeo = Just geo'org
      | otherwise   = Nothing

flipGeo :: Geo -> Geo
flipGeo (Geo w h) = Geo h w

-- the r.e. is more general, the geo spec maybe surrounded
-- by arbitrary other stuff (used in parsing exif data)

geoRegex :: Regex
geoRegex = parseRegexExt geoRegex'

geoRegex'
  , geoRegex'' :: String

geoRegex'  = "[^1-9]*" ++ geoXY ++ "[^0-9]*"
geoRegex'' = "([1-9][0-9]*)x([1-9][0-9]*)" ++ "|" ++ orgGeo

geoXY :: String
geoXY = "({w}[1-9][0-9]*)x({h}[1-9][0-9]*)"

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

arRegex :: Regex
arRegex = parseRegexExt arRegex'

arRegex' :: String
arRegex' =
  intercalate "|" $
  map (^. isoString) [minBound .. maxBound :: AspectRatio]

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

geoarRegex :: Regex
geoarRegex = parseRegexExt geoarRegex'

geoarRegex' :: String
geoarRegex' = "(({ar}" ++ arRegex' ++ ")-)?({geo}" ++ geoRegex'' ++ ")"

readGeoAR :: String -> Maybe GeoAR
readGeoAR = build . matchSubexRE geoarRegex
  where
    build [("ar", ar'), ("geo", geo')] =
      let ar  = ar'  ^. from isoString
          geo = geo' ^. from isoString
      in
        Just $ (geo, ar) ^. from geoar2pair
    build [("geo", geo')] =
        Just $ (geo' ^. from isoString, Pad) ^. from geoar2pair
    build _ =
      Nothing

isoGeoAR :: Iso' String (Maybe GeoAR)
isoGeoAR = iso readGeoAR (maybe "" (^. isoString))

-- ----------------------------------------
