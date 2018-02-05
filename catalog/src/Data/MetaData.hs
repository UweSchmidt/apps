{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
where

import           Data.Prim
import qualified Data.Aeson          as J
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.Scientific     as SC
import           Text.Printf         ( printf )
import           Text.Read           ( readMaybe )
-- import Debug.Trace

-- ----------------------------------------

newtype MetaData = MD J.Object

-- ----------------------------------------

deriving instance Show MetaData

instance Monoid MetaData where
  mempty  = MD HM.empty
  mappend = mergeMD
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

foldWithKeyMD :: (Name -> Text -> a -> a) -> a -> MetaData -> a
foldWithKeyMD f acc m@(MD hm) =
  foldl' f' acc keys
  where
    keys :: [Name]
    keys =
      map (^. from isoText) (HM.keys hm)

    f' acc' n' =
      f n' (m ^. metaDataAt n') acc'

-- merging of meta data
-- default: entries of m1 overwrite entries of m2
-- if an attr value in m1 is "-", the attribute is deleted in the result
-- if an attr value in m1 is "", the m2 value is taken
-- in keywords the "," separated list of keywords is computed
-- and the union of the sets of keywords is taken,
-- but all keywords in m1 starting with a "-" are removed

mergeMD :: MetaData -> MetaData -> MetaData
mergeMD m1 m2 =
  foldWithKeyMD mergeAttr m2 m1
  where
    mergeAttr :: Name -> Text -> MetaData -> MetaData
    mergeAttr n v acc
      | v == "-"  = acc & metaDataAt n .~ ""   -- remove attr
      | T.null v  = acc                        -- attribute not changed
      | n == descrKeywords
                  = acc & metaDataAt n %~ mergeKeywords v
      | n == descrRating
                  = acc & metaDataAt n .~ (v ^. isoString . isoRating
                                           .
                                           to (\ r -> if r == 0
                                                      then ""
                                                      else show r
                                              )
                                           .
                                           isoText
                                          )
      | otherwise = acc & metaDataAt n .~ v

mergeKeywords :: Text -> Text -> Text
mergeKeywords t1 t2 =
  T.intercalate "," kw
  where
    kw = (kw2 L.\\ rmv) `L.union` nub ins
    kws = map T.strip . T.split (== ',')
    kw1 = kws t1
    kw2 = kws t2
    (rmv', ins) = partition ((== "-"). T.take 1) kw1
    rmv = map (T.drop 1) rmv'

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
        totext (Just (J.Number n)) = showSc n ^. from isoString
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
--
-- manipulate the access attributes in MetaData

modifyAccess :: (Text -> Text) -> MetaData -> MetaData
modifyAccess f md =
  md & metaDataAt descrAccess %~ f

setAccess :: [Text] -> MetaData -> MetaData
setAccess ts = modifyAccess (const $ T.unwords ts)

allowAccess :: [Text] -> MetaData -> MetaData
allowAccess ts = modifyAccess f
  where
    f= T.unwords . filter (`notElem` ts) . T.words

restrAccess :: [Text] -> MetaData -> MetaData
restrAccess ts = modifyAccess f
  where
    f = T.unwords . nub . (ts ++) . T.words

clearAccess
  , addNoWriteAccess
  , addNoSortAccess
  , addNoDeleteAccess
  , subNoWriteAccess
  , subNoSortAccess
  , subNoDeleteAccess :: MetaData -> MetaData

clearAccess       = setAccess []
addNoWriteAccess  = restrAccess [no'write]
addNoSortAccess   = restrAccess [no'sort]
addNoDeleteAccess = restrAccess [no'delete]
subNoWriteAccess  = allowAccess [no'write]
subNoSortAccess   = allowAccess [no'sort]
subNoDeleteAccess = allowAccess [no'delete]

getAccess :: ([Text] -> Bool) -> MetaData -> Bool
getAccess f md =
  md ^. metaDataAt descrAccess . to (f . T.words)

isWriteable
  , isSortable, isRemovable :: MetaData -> Bool
isWriteable = getAccess (no'write  `notElem`)
isSortable  = getAccess (no'sort   `notElem`)
isRemovable = getAccess (no'delete `notElem`)

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

getOrientation :: MetaData -> Int
getOrientation md =
  md ^. metaDataAt "EXIF:Orientation" . to toOri
  where
    toOri :: Text -> Int
    toOri t
      | t == "Rotate 90 CW"  = 1
      | t == "Rotate 180 CW" = 2
      | t == "Rotate 90 CCW" = 3
      | t == "Rotate 270 CW" = 3
      | otherwise            = 0

type Rating = Int -- 0 .. 5

ratingMax :: Rating
ratingMax = 5

getRating :: MetaData -> Rating
getRating md =
  lookupByNames
  [ descrRating     -- descr:Rating has priority over
  , "XMP:Rating"    -- XMP:Rating from LR
  ] md
  ^. isoString . isoRating

isoRating :: Iso' String Rating
isoRating = iso fromS show
  where
    fromS s = min ratingMax $ i1 `max` i2
      where
        i1 = max 0 . fromMaybe 0 . readMaybe $ s
        i2 = s ^. from isoStars

isoStars :: Iso' Rating String
isoStars = iso (flip replicate '*')
               (min ratingMax . length . filter (== '*'))

mkRating :: Rating -> MetaData
mkRating r = mempty & metaDataAt descrRating .~ (r ^. isoString . isoText)

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
filterMetaData IMGjpg  m = m ^. selectByRegex reRaw
filterMetaData IMGimg  m = m ^. selectByRegex reRaw
filterMetaData _       _ = mempty

-- ----------------------------------------
--
-- meta data parsers

type YMD = (String, String, String)
type HMS = (String, String, String, String)
type YMD'HMS = (YMD, HMS)

parseDateTime :: String -> Maybe YMD'HMS
parseDateTime str = do
  (ymd, hms) <- parseDateTime' str
  ymd'       <- checkYMD ymd
  hms'       <- checkHMS hms
  return (ymd', hms')
  where
    checkYMD x@(y', m', d')
      | y >= 1900 && y < 3001
        &&
        m >= 1 && m <= 12
        &&
        d >= 1 && d <= 31 =
          Just x
      | otherwise =
          Nothing
      where
        y, m, d :: Int
        y = read y'
        m = read m'
        d = read d'

    checkHMS x@(h', m', s', _ms')
      | h >= 0 && h <= 24
        &&
        m >= 0 && m < 60
        &&
        s >= 0 && s < 60 =
          Just x
      | otherwise =
          Nothing
      where
        h, m, s :: Int
        h = read h'
        m = if null m' then 0 else read m'
        s = if null s' then 0 else read s'

parseDateTime' :: String -> Maybe YMD'HMS
parseDateTime' str =
  case res of
    -- just year, month, day
    [("Y",y), ("M",m), ("D",d)] ->
      Just ((y, m, d), ("", "", "", ""))

    -- date and time, without msec
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
  "({Y}[1-9][0-9]{3})[-:]({M}[0-9]{2})[-:]({D}[0-9]{2})"
  ++ "("
  ++ "[ ]+"
  ++ "({h}[0-9]{2}):({m}[0-9]{2}):({s}[0-9]{2})({ms}[.][0-9]+)?"
  ++ ")?"
  ++ "([^0-9].*)?"

-- take the day part from a date/time input
parseDate :: String -> Maybe (String, String, String)
parseDate str = fst <$> parseDateTime str
{-# INLINE parseDate #-}

isoDateInt :: Iso' (String, String, String) Int
isoDateInt = iso toInt frInt
  where
    toInt (y, m, d) =
      (read y * 100 + read m) * 100 + read d

    frInt i = ( printf "%04d" y
              , printf "%02d" m
              , printf "%02d" d
              )
      where
        (my, d) = i  `divMod` 100
        (y,  m) = my `divMod` 100

-- take the time part of a full date/time input
parseTime :: String -> Maybe (String, String, String, String)
parseTime str = do
  (_, t) <- parseDateTime str
  case t of
    ("", "", "", "") -> mzero
    _                -> return t

-- ----------------------------------------

reDeg :: Regex
reDeg = parseRegexExt $
  "({deg}[0-9]+) +deg +({min}[0-9]+)' +({sec}[.0-9]+)\" +({dir}[NWES])"

reLongLat :: Regex
reLongLat = parseRegexExt $
  "({lat}[^NS]+[NS]),? +({long}[^WE]+[WE])"

-- | "degrees minutes seconds" to "decimal degrees" (google url format)
parseDeg :: String -> Maybe (Int, Int, Double, Char)
parseDeg loc =
  case matchSubexRE reDeg loc of
    [("deg", deg), ("min", mn), ("sec", sec), ("dir", dir)] ->
      Just (read deg, read mn, read sec, head dir)
    _ ->
      Nothing

deg2DegDec :: (Int, Int, Double, Char) -> Double
deg2DegDec (d, m, s, dir) =
  (d' + m'/60 + s/3600) *
  ( if dir == 'W'
       ||
       dir == 'S'
    then (0-1)
    else 1
  )
  where
    d' = fromIntegral d
    m' = fromIntegral m

latLong2DegDec :: String -> Maybe (Double, Double)
latLong2DegDec loc =
  case matchSubexRE reLongLat loc of
     [("lat", lat), ("long", long)] -> do
       lat'  <- deg2DegDec <$> parseDeg lat
       long' <- deg2DegDec <$> parseDeg long
       return (lat', long')
     _ ->
       Nothing

latLong2googleMapsUrl :: (Double, Double) -> String
latLong2googleMapsUrl (lat, long) =
  lat' ++ "," ++ long'
  where
    format = printf "%12.9f"
    lat'   = format lat
    long'  = format long

loc2googleMapsUrl :: String -> Maybe String
loc2googleMapsUrl loc =
  latLong2googleMapsUrl <$> latLong2DegDec loc

-- ----------------------------------------

type AttrGroup = (Text, [Text])

attrGroups2regex :: [AttrGroup] -> RegexText
attrGroups2regex =
  parseRegex' .
  mkAlt .
  map (\ (px, attr) -> (px ^. isoString ++ ":" ++ mkAlt (map (^. isoString) attr)))
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

px2a :: Text -> [Name]
px2a s = map (^. from isoText) ag20
{-}
  case ag20 of
    [x1] -> mkName x1
    []   -> mempty
    xs   -> error $ "ambigious name abreviation " ++ show s ++ " matches " ++ show xs
-- -}
  where
    g, n :: Text
    (g, n) | T.null n'   = ("", g')
           | otherwise = (g', T.tail n')
      where
        (g', n') = T.span (/= ':') s

    filterNotNull =
      filter (not . null .snd)

    ag20 :: [Text]
    ag20 = concatMap (\ (x, xs) -> map ((x <> ":") <>) xs) ag11
    -- exact name matches are prefered

    ag11 :: [AttrGroup]
    ag11
      | null ag10 = filterNotNull $
                    map (second (filter (n `T.isPrefixOf`))) ag01
      | otherwise = ag10

    ag10 :: [AttrGroup]
    ag10 = filterNotNull $
      map (second (filter (== n))) ag01

    -- exact group matches are prefered
    ag01 :: [AttrGroup]
    ag01
      | null ag00 = filter ((g `T.isPrefixOf`) . fst) allAttrGroups
      | otherwise = ag00

    ag00 :: [AttrGroup]
    ag00
      | T.null g  = allAttrGroups
      | otherwise = filter ((== g) .fst) allAttrGroups

-- ----------------------------------------

attrGroup2attrName :: AttrGroup -> [Name]
attrGroup2attrName (px, as) = map (\ a -> (px <> ":" <> a) ^. from isoText) as

allAttrGroups :: [AttrGroup]
allAttrGroups =
  [ attrFile
  , attrExif
  , attrMaker
  , attrComposite
  , attrXmp
  , attrCol
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
    , "FocusDistance"
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
    , "GPSAltitude"
    , "Format"
    , "RawFileName"
    , "Rating"
    ]
  )

attrCol :: AttrGroup
attrCol =
  ( "descr"
  , [ "Title"
    , "Subtitle"
    , "TitleEnglish"
    , "TitleLatin"
    , "Keywords"
    , "Web"
    , "Wikipedia"
    , "GoogleMaps"
    , "Comment"
    , "CreateDate"
    , "OrderedBy"
    , "Access"
    , "Duration"
    , "Rating"
    ]
  )
descrTitle
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrGoogleMaps
  , descrComment
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating :: Name

[descrTitle
  , descrSubtitle
  , descrTitleEnglish
  , descrTitleLatin
  , descrKeywords
  , descrWeb
  , descrWikipedia
  , descrGoogleMaps
  , descrComment
  , descrCreateDate
  , descrOrderedBy
  , descrAccess
  , descrDuration
  , descrRating
  ] = attrGroup2attrName attrCol

-- ----------------------------------------
