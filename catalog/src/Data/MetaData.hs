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
import           Text.SimpleParser
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

selectByParser :: SP String -> Lens' MetaData MetaData
selectByParser ps = selectMetaData p
  where
    p n = matchP ps (n ^. isoString)
{-# INLINE selectByParser #-}

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
getCreateMeta parse' md =
  parse' cd
  where
    cd = lookupByNames
      [ "Composite:SubSecCreateDate"
      , "EXIF:CreateDate"
      ] md
      ^. isoString

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
filterMetaData IMGraw  m = m ^. selectByParser psRaw
filterMetaData IMGmeta m = m ^. selectByParser psXmp
filterMetaData IMGjpg  m = m ^. selectByParser psRaw
filterMetaData IMGimg  m = m ^. selectByParser psRaw
filterMetaData _       _ = mempty

-- ----------------------------------------
--
-- meta data parsers

type YMD = (String, String, String)
type HMS = (String, String, String, String)
type YMD'HMS = (YMD, HMS)

parseDateTime :: String -> Maybe YMD'HMS
parseDateTime = parseMaybe dateTimeParser

-- take the day part from a date/time input
parseDate :: String -> Maybe (String, String, String)
parseDate = parseMaybe (fst <$> dateTimeParser)
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
parseTime = parseMaybe (snd <$> dateTimeParser)
{-# INLINE parseTime #-}

timeParser :: SP HMS
timeParser = do
  h  <-             count 2 digitChar
  m  <- char ':' *> count 2 digitChar
  s  <- char ':' *> count 2 digitChar
  ms <- option ".0" $
        char '.' *> some    digitChar
  let (h', m', s') = (read h, read m, read s) :: (Int, Int, Int)
  if h' >= 0 && h' <= 24
     &&
     m' >= 0 && m' <  60
     &&
     s' >= 0 && s' <  60
    then return (h, m, s, ms)
    else mzero

dateParser :: SP YMD
dateParser = do
  y <-              count 4 digitChar
  m <- oneOf del *> count 2 digitChar
  d <- oneOf del *> count 2 digitChar
  let (y', m', d') = (read y, read m, read d) :: (Int, Int, Int)
  if y' >= 1800 && y' < 3001
     &&
     m' >= 1    && m' <= 12
     &&
     d' >= 1    && d' <= 31
    then return (y, m, d)
    else mzero
  where
    del :: String
    del = "-:"

dateTimeParser :: SP YMD'HMS
dateTimeParser = do
  ymd <- dateParser
  hms <- some spaceChar *> timeParser <* anyString  -- maybe followed by time zone
  return (ymd, hms)

-- ----------------------------------------

-- the old regex for matchSubRE was much short, but error phrone
-- no parse in read::Double and other bugs

degParser :: String -> SP (Int, Int, Double, Char)
degParser dirs = do
  deg <- read <$> (some digitChar <* sp <* string "deg" <* sp)
  mn  <- read <$> (some digitChar <* char '\''          <* sp)
  sec <- read <$> (float          <* char '"'           <* sp)
  dir <- oneOf dirs
  return (deg, mn, sec, dir)
  where
    sp :: SP String
    sp = some (char ' ')

    float :: SP String
    float =
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

-- parse latitute and longitude
-- and convert to decimal degees

latLongParser :: SP (Double, Double)
latLongParser = do
  lat  <- deg2DegDec <$>         degParser "NS"
  long <- deg2DegDec <$> (del *> degParser "WE")
  return (lat, long)
  where
    del = option ' ' (char ',') *> some (char ' ')


-- | "degrees minutes seconds dir" to "decimal degrees" (google url format)

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

latLong2googleMapsUrl :: (Double, Double) -> String
latLong2googleMapsUrl (lat, long) =
  lat' ++ "," ++ long'
  where
    format = printf "%12.9f"
    lat'   = format lat
    long'  = format long

loc2googleMapsUrl :: String -> Maybe String
loc2googleMapsUrl loc =
  latLong2googleMapsUrl <$> parseMaybe latLongParser loc

-- ----------------------------------------

type AttrGroup = (Text, [Text])

psRaw, psXmp :: SP String

psRaw = attrGroupsParser
  [ attrExif
  , attrComposite
  , attrMaker
  , attrFile
  ]

psXmp = attrGroupsParser
  [ attrComposite
  , attrXmp
  ]

attrGroupsParser :: [AttrGroup] -> SP String
attrGroupsParser =
  foldr1 (<|>) . map toP
  where
    toP :: AttrGroup -> SP String
    toP (px, attr) =
      try ( string (px ^. isoString)
            <++>
            string ":"
            <++>
            foldr1 (<|>) (map toA attr)
          )

    toA :: Text -> SP String
    toA a = try (string (a ^. isoString) <* eof)

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
