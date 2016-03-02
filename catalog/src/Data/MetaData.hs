{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
where

import           Control.Lens
import           Control.Lens.Util
import           Control.Monad
import           Data.Prim

import qualified Data.Aeson          as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V

-- ----------------------------------------

newtype MetaData = MD J.Object

-- ----------------------------------------

deriving instance Show MetaData

instance Monoid MetaData where
  mempty                = emptyMetaData
  MD m1 `mappend` MD m2 = MD $ m1 `HM.union` m2
  -- the left map entries are prefered

instance ToJSON MetaData where
  toJSON (MD m) = J.toJSON [m]

instance FromJSON MetaData where
  parseJSON = J.withArray "MetaData" $ \ v ->
    case V.length v of
      1 -> J.withObject "MetaData" (return . MD) (V.head v)
      _ -> mzero

emptyMetaData :: MetaData
emptyMetaData = MD HM.empty

nullMetaData :: MetaData -> Bool
nullMetaData (MD m) = HM.null m

-- ----------------------------------------
--
-- MetaData lenses

metaDataAt :: Name -> Lens' MetaData Text
metaDataAt key = md2obj . at (key ^. name2text) . val2text
  where
    md2obj :: Iso' MetaData J.Object
    md2obj = iso (\ (MD m) -> m) MD

    val2text :: Iso' (Maybe J.Value) Text
    val2text = iso totext fromtext
      where
        totext (Just (J.String t)) = t
        totext (Just (J.Number n)) = (show n) ^. isoStringText
        totext _                   = ""

        fromtext t
          | T.null t = Nothing
          | otherwise = Just (J.String t)


partMetaData :: (Name -> Bool) -> Iso' MetaData (MetaData, MetaData)
partMetaData predicate = iso part (uncurry mappend)
  where
    part (MD m) = (MD *** MD) $ HM.foldrWithKey pf (HM.empty, HM.empty) m
      where
        pf k v (m1, m2)
          | predicate (k ^. from name2text) =
              (HM.insert k v m1, m2)
          | otherwise =
              (m1, HM.insert k v m2)

selectMetaData :: (Name -> Bool) -> Lens' MetaData MetaData
selectMetaData p = partMetaData p . _1

selectByRegex :: RegexText -> Lens' MetaData MetaData
selectByRegex rx' = selectMetaData p
  where
    p n = matchRE rx' (n ^. name2text)

selectByNames :: [Name] -> Lens' MetaData MetaData
selectByNames ns = selectMetaData (`elem` ns)

-- lookup a sequence of fields and take first value found
lookupByNames :: [Name] -> MetaData -> Text
lookupByNames ns md =
  head (vs ++ [T.empty])
  where
    vs = filter (not . T.null) $
         map (\ n -> md ^. metaDataAt n) ns

-- ----------------------------------------

filterMetaData :: ImgType -> MetaData -> MetaData
filterMetaData IMGraw  m = m ^. selectByRegex reRaw
filterMetaData IMGmeta m = m ^. selectByRegex reXmp
filterMetaData IMGimg  m = m ^. selectByRegex reRaw
filterMetaData _       _ = emptyMetaData

-- ----------------------------------------

type AttrGroup = (String, [String])

attrGroups2regex :: [AttrGroup] -> RegexText
attrGroups2regex =
  parseRegex' .
  mkAlt .
  map (\ (px, attr) -> (px ++ ":" ++ mkAlt attr))
  where
    mkPar :: String -> String
    mkPar s = "(" ++ s ++ ")"

    mkAlt :: [String] -> String
    mkAlt xs = mkPar $ intercalate "|" $ map mkPar xs



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
  , [ "Directory"
    , "FileSize"
    , "FileModifyDate"
    , "FileName"
    , "MIMEType"
    ]
  )

attrExif :: AttrGroup
attrExif =
  ( "EXIF"
  , [ "Artist"
    , "BitsPerSample"
    , "Copyright"
    , "CreateDate"
    , "ExposureCompensation"
    , "ExposureMode"
    , "ExposureProgram"
    , "ExposureTime"
    , "Flash"
    , "FNumber"
    , "FocalLength"
    , "FocalLengthIn35mmFormat"
    , "GPSVersionID"
    , "ImageHeight"
    , "ImageWidth"
    , "ISO"
    , "Make"
    , "MaxApertureValue"
    , "MeteringMode"
    , "Model"
    , "Orientation"
    , "UserComment"
    , "WhiteBalance"
    ]
  )

attrMaker :: AttrGroup
attrMaker =
  ( "MakerNotes"
  , [ "ColorSpace"
    , "DaylightSavings"
    , "FocusMode"
    , "Quality"
    , "SerialNumber"
    , "ShootingMode"
    , "ShutterCount"
    , "TimeZone"
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

-- ----------------------------------------
