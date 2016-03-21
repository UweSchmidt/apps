{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
where

import           Control.Monad
import           Data.Prim

import qualified Data.Aeson          as J
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Scientific     as SC

-- ----------------------------------------

newtype MetaData = MD J.Object

-- ----------------------------------------

deriving instance Show MetaData

instance Monoid MetaData where
  mempty                = MD HM.empty
  MD m1 `mappend` MD m2 = MD $ m1 `HM.union` m2
  -- the left map entries are prefered
  {-# INLINE mappend #-}
  {-# INLINE mempty  #-}

instance IsEmpty MetaData where
  isempty (MD md) = HM.null md
  {-# INLINE isempty #-}

instance ToJSON MetaData where
  toJSON (MD m) = J.toJSON [m]
  {-# INLINE toJSON #-}

instance FromJSON MetaData where
  parseJSON = J.withArray "MetaData" $ \ v ->
    case V.length v of
      1 -> J.withObject "MetaData" (return . MD) (V.head v)
      _ -> mzero

-- ----------------------------------------
--
-- MetaData lenses

metaDataAt :: Name -> Lens' MetaData Text
metaDataAt key = md2obj . at (key ^. isoText) . val2text
  where
    md2obj :: Iso' MetaData J.Object
    md2obj = iso (\ (MD m) -> m) MD
    {-# INLINE md2obj #-}

    {-# INLINE val2text #-}
    val2text :: Iso' (Maybe J.Value) Text
    val2text = iso totext fromtext
      where
        totext (Just (J.String t)) = t
        totext (Just (J.Number n)) = (showSc n) ^. from isoString
        totext _                   = ""

        fromtext t
          | isempty t = Nothing
          | otherwise = Just (J.String t)

        showSc n =
          either showF showI $ SC.floatingOrInteger n
          where
            showF :: Double -> String
            showF _ = show n

            showI :: Integer -> String
            showI = show

{-# INLINE metaDataAt #-}

partMetaData :: (Name -> Bool) -> Iso' MetaData (MetaData, MetaData)
partMetaData predicate = iso part (uncurry mappend)
  where
    part (MD m) = (MD *** MD) $ HM.foldrWithKey pf (HM.empty, HM.empty) m
      where
        pf k v (m1, m2)
          | predicate (k ^. from isoText) =
              (HM.insert k v m1, m2)
          | otherwise =
              (m1, HM.insert k v m2)
{-# INLINE partMetaData #-}

selectMetaData :: (Name -> Bool) -> Lens' MetaData MetaData
selectMetaData p = partMetaData p . _1
{-# INLINE selectMetaData #-}

selectByRegex :: RegexText -> Lens' MetaData MetaData
selectByRegex rx' = selectMetaData p
  where
    p n = matchRE rx' (n ^. isoText)
{-# INLINE selectByRegex #-}

selectByNames :: [Name] -> Lens' MetaData MetaData
selectByNames ns = selectMetaData (`elem` ns)
{-# INLINE selectByNames #-}

-- lookup a sequence of fields and take first value found
lookupByNames :: [Name] -> MetaData -> Text
lookupByNames ns md =
  head (vs ++ [T.empty])
  where
    vs = filter (not . isempty) $
         map (\ n -> md ^. metaDataAt n) ns

-- ----------------------------------------

getCreateMeta :: (String -> res) -> MetaData -> res
getCreateMeta parse md =
  parse cd
  where
    cd = lookupByNames
      [ "Composite:SubSecCreateDate"
      , "EXIF:CreateDate"
      ] md
      ^. isoString

--    res = matchSubexRE reDateTime $ cd ^. isoString

getFileName :: MetaData -> Maybe Text
getFileName md =
  md ^. metaDataAt "File:Filename" . isoMaybe
{-# INLINE getFileName #-}

-- ----------------------------------------
--
-- compare function on meta data

compareByCreateDate :: MetaData -> MetaData -> Ordering
compareByCreateDate =
  compareBy [ compareJust' `on` getCreateMeta parseDateTime
            , compare      `on` getFileName
            ]
{-# INLINE compareByCreateDate #-}

compareByName :: MetaData -> MetaData -> Ordering
compareByName =
  compareBy [ compare `on` getFileName
            ]
{-# INLINE compareByName #-}

-- ----------------------------------------
--
-- filter meta data enries by image type

filterMetaData :: ImgType -> MetaData -> MetaData
filterMetaData IMGraw  m = m ^. selectByRegex reRaw
filterMetaData IMGmeta m = m ^. selectByRegex reXmp
filterMetaData IMGimg  m = m ^. selectByRegex reRaw
filterMetaData _       _ = mempty

-- ----------------------------------------
--
-- meta data parsers

parseDateTime :: String -> Maybe ( (String, String, String)
                                 , (String, String, String, String)
                                 )
parseDateTime str =
  case res of
    -- just year, month, day
    [("Y",y), ("M",m), ("D",d)] ->
      Just ((y, m, d), ("", "", "", ""))

    -- date and time, witoutt msec
    [("Y",y), ("M",m), ("D",d), ("h", h), ("m", mi), ("s", s)] ->
      Just ((y, m, d), (h, mi, s, ""))

    -- date and time with msec
    [("Y",y), ("M",m), ("D",d), ("h", h), ("m", mi), ("s", s), ("ms",ms)] ->
      Just ((y, m, d), (h, mi, s, ms))

    -- no match
    _ -> Nothing
  where
    res = matchSubexRE reDateTime str

reDateTime :: Regex
reDateTime = parseRegexExt $
  "({Y}[0-9]{4})[-:]({M}[0-9]{2})[-:]({D}[0-9]{2})"
  ++ "("
  ++ "[ ]+"
  ++ "({h}[0-9]{2}):({m}[0-9]{2}):({s}[0-9]{2})({ms}[.][0-9]+)?"
  ++ ")?"
  ++ "([^0-9].*)?"

-- take the day part from a date/time input
parseDate :: String -> Maybe (String, String, String)
parseDate str = fst <$> parseDateTime str
{-# INLINE parseDate #-}

-- take the time part of a full date/time input
parseTime :: String -> Maybe (String, String, String, String)
parseTime str = do
  (_, t) <- parseDateTime str
  case t of
    ("", "", "", "") -> mzero
    _                -> return t

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
    []   -> mempty
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

attrCol :: AttrGroup
attrCol =
  ( "COL"
  , [ "Title"
    , "Subtitle"
    , "Comment"
    , "Resource"
    , "CreateDate"
    , "OrderedBy"
    , "Access"
    ]
  )

-- ----------------------------------------
