-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Applicative
import Data.Prim hiding (noneOf)
import Text.Megaparsec
import Text.Megaparsec.Char

-- ----------------------------------------

type FnameParser = FilePath -> Maybe (NameImgType, Name)

type FilePathConfig' = [FnameParser]

fpToImgType' :: (ImgType -> Bool)
             -> FilePathConfig'
             -> FilePath -> (NameImgType, Name)
fpToImgType' tp conf path =
  fromMaybe defRes $
  foldl1 (<|>) $ map parse' conf
  where
    defRes = ((mkName path, IMGother), mempty)
    parse' p = p path >>= matchPred (tp . snd . fst)

toFilePathConfig' :: FilePathConfig -> FilePathConfig'
toFilePathConfig' =
  map toCF'
  where
    toCF' :: (Regex, ImgType) -> FnameParser
    toCF' (rex, ty) = parse'
      where
        parse' fn' =
          partRes $  matchSubexRE rex fn'
          where
            partRes [("1", base)] =
              Just ((mkName base, ty), mempty)
            partRes [("1", base), ("2", ext)] =
              Just ((mkName base, ty), mkName ext)
            partRes _ =
              Nothing

filePathToImgType' :: FilePath -> NameImgType
filePathToImgType' =
  fst . fpToImgType' (const True) (toFilePathConfig' filePathConfig)

filePathToExt' :: ImgType -> FilePath -> Name
filePathToExt' ty =
  snd . fpToImgType' (== ty) (toFilePathConfig' filePathConfig)

-- --------------------

type FilePathConfig = [(Regex, ImgType)]

filePathToImgType :: FilePath -> NameImgType
filePathToImgType = fst . fpToImgType filePathConfig

filePathToExt :: ImgType -> FilePath -> Name
filePathToExt ty = snd . fpToImgType fpc
  where
    fpc = filter ((== ty) . snd) filePathConfig

fpToImgType :: FilePathConfig -> FilePath -> (NameImgType, Name)
fpToImgType conf path =
  fromMaybe ((mkName path, IMGother), mempty) $
  foldr1 (<|>) $
  map parse' conf
  where
    parse' (e, ty) =
      partRes $  matchSubexRE e path
      where

        partRes [("1", base)] =
          Just ((mkName base, ty), mempty)
        partRes [("1", base), ("2", ext)] =
          Just ((mkName base, ty), mkName ext)
        partRes _ =
          Nothing

filePathConfig :: FilePathConfig
filePathConfig = map (first parseRegexExt) $
  [ (mk1 boringName,               IMGboring)
  , (mk1 baseName  ++ mk2 rawExt,  IMGraw)
  , (mk1 baseName  ++ mk2 jpgExt,  IMGjpg)  -- this jpg is the original image
  , (mk1 baseName  ++ mk2 imgExt,  IMGimg)
  , (mk1 baseName  ++ mk2 xmpExt,  IMGmeta)
  , (mk1 baseName  ++ mk2 jsonExt, IMGjson)
  , (mk1 baseName  ++ mk2 ptoExt,  IMGhugin)
  , (mk1 baseName  ++ mk2 dxoExt,  IMGdxo)
  , (mk1 baseName  ++ mk2 txtExt,  IMGtxt)
  , (mk1 baseName  ++ mk2 dngExt,  IMGdng)
  , (mk1 jpgdirName,               IMGjpgdir)
  , (mk1 imgdirName,               IMGimgdir)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 (geoExt ++ jpgExt),    IMGcopy)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 jpgExt,                IMGjpg)  -- these jpgs are developed from a raw image
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 dngExt,                IMGdng)  -- these dngs are developed from a raw image
  ]
  where
    mk1  e = "({1}(" ++ e ++ "))"
    mk2  e = "({2}(" ++ e ++ "))"

    baseName  = "[-+._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF|rw2|RW2)"
    imgExt    = "[.](gif|tiff?|png|ppm|pgm|pbm)"
    xmpExt    = "[.](xmp|XMP)"
    dxoExt    = "[.]((nef|NEF|rw2|RW2|jpg|JPG)[.]dop)"
    ptoExt    = "[.]pto"
    jsonExt   = "[.]json"
    dngExt    = "[.](dng|DNG)"
    geoExt    = "[.]([0-9]+x[0-9]+)"
    txtExt    = "[.](md|txt)"

    imgdirName = "[-+._A-Za-z0-9]+" -- no do
    jpgdirName =
      "("
      ++ ( intercalate "|"
           [ "srgb[0-9]*"
           , "srgb" ++ "(-bw)?" ++ og
           , "[0-9]+x[0-9]+"
           , "dxo"
           , "small" ++ og
           , "web"   ++ og
           , "bw"    ++ og
           , "jpg"   ++ og
           , "tiff?" ++ og
           , "dng"   ++ og
           ]
         )
      ++ ")"
      where
        og = "(-?([0-9]+(x[0-9]+)?))?"

    jpgdirPre =
      "(" ++ jpgdirName ++ "/)?"

    boringName = intercalate "|"
      [ "[.].*"
      , ".*~"
      , "tmp.*"
      , ".*[.](bak|old)"
      ]

jpgExt :: String
jpgExt = "[.](jpg|JPG)"

jpgPath :: String
jpgPath = "/.*" ++ jpgExt

-- ----------------------------------------
--
-- these regex must match the filePathConfig expressions
-- if new IMGimg or IMGtxt extensions are added, these regex
-- must be updated

txtSrcExpr :: Regex
txtSrcExpr =
  parseRegexExt txtpathRE

imgSrcExpr :: Regex
imgSrcExpr =
  parseRegexExt $ "({path}/.*[.](gif|png|tiff?)|ppm|pgm|pbm)[.]jpg"

txtPathExpr :: Regex
txtPathExpr =
  parseRegexExt $
  geoarRE ++ topdirRE ++ txtpathRE

imgPathExpr :: Regex
imgPathExpr =
  parseRegexExt $
  geoarRE ++ topdirRE ++ "({path}" ++ jpgPath ++ ")"

geoarRE :: String
geoarRE =
  "/({geoar}" ++ ar ++ "-" ++ ge ++ ")"
  where
    ar  = "(" ++ ar' ++ ")"
    ar' = intercalate "|" $ map (^. isoString) $ ars

    ge  = "([0-9]+x[0-9]+|org)"

    ars :: [AspectRatio]
    ars = [minBound..maxBound]

topdirRE :: String
topdirRE = "({topdir}/[^/]+)"

txtpathRE :: String
txtpathRE = "({path}/.*[.](txt|md))"

-- extract the path component from a file path

objSrc :: Regex -> FilePath -> FilePath
objSrc oex p =
  case matchSubexRE oex p of
    [("path", path)] ->
      path
    _ ->
      p

imgExtExpr :: Regex
imgExtExpr =
  parseRegex ".*[.](nef|NEF||rw2|RW2|dng|DNG|png|PNG|jpg|JPG|xmp|XMP)"

-- ----------------------------------------

pathToBreadCrump :: String -> String
pathToBreadCrump = sed (const " \8594 ") "/" . drop 1

-- ----------------------------------------
--
-- url pasers without regex matching

type SP = Parsec () String

splitLast :: [a] -> Maybe ([a], a)
splitLast [x]      = Just ([], x)
splitLast (x : xs) = first (x:) <$> splitLast xs
splitLast []       = Nothing

joinLast :: [a] -> a -> [a]
joinLast xs x = xs ++ [x]

-- --------------------

splitPath :: String -> Maybe [String]
splitPath = parseMaybe pPath
  where
    pPath :: SP [String]
    pPath = do
      ps <- filter (not . null) <$> many piece
      case ps of
        [] -> mzero
        _  -> return ps

    piece = char '/' >> many (noneOf "/")

joinPath :: [String] -> String
joinPath = concatMap ('/' :)

-- --------------------

-- split a filename into basename and list of extensions
--
-- splitExt "abc.def"     -> Just ["abc", ".def"]
-- splitExt "abc.def.ghi" -> Just ["abc", ".def", ".ghi"]
-- splitExt "abc"         -> Nothing
-- splitExt "abc"         -> Nothing
-- splitExt ".iii"        -> Nothing
-- splitExt "abc..ii"     -> Nothing

splitExt :: String -> Maybe [String]
splitExt = parseMaybe pExt
  where
    pExt :: SP [String]
    pExt = do
      p1 <- part
      ps <- some ext
      return (p1 : ps)

    part = some $ noneOf "."
    ext  = ('.' :) <$> (char '.' >> part)

joinExt :: [String] -> String
joinExt = concat

-- splitDirFileExt "/xxx/abc.jpg" -> Just ("/xxx","abc",".jpg")
-- splitDirFileExt "/abc.jpg"     -> Just ("","abc",".jpg")
-- splitDirFileExt "/abc.txt.jpg" -> Just ("","abc.txt",".jpg")

splitDirFileExt :: String -> Maybe (String, String, String)
splitDirFileExt xs = do
  (dp, fn) <- first joinPath <$> (splitPath xs >>= splitLast)
  (bn, ex) <- first joinExt  <$> (splitExt  fn >>= splitLast)
  return (dp, bn, ex)


-- splitDirFileExt2 "/abc.txt.jpg" -> Just ("","abc",".jpg",".txt")
-- splitDirFileExt2 "/abc.jpg"     -> Nothing

splitDirFileExt2 :: String -> Maybe (String, String, String, String)
splitDirFileExt2 xs = do
  (dp, fn, ex1) <- splitDirFileExt xs
  (bn, ex2)     <- first joinExt <$> (splitExt fn >>= splitLast)
  return (dp, bn, ex2, ex1)

matchExt :: ImgType -> String -> String -> Maybe ImgType
matchExt ty ex xs = matchPred (eqNoCase ex) xs >> return ty

matchExts :: ImgType -> [String] -> String -> Maybe ImgType
matchExts ty exs xs = matchPred (\ ys -> any (eqNoCase ys) exs) xs >> return ty

extImg, extJpg, extRaw, extDng, extTxt,
  extXmp, extDxO, extPto,
  extJson :: String -> Maybe ImgType

extImg  = matchExts IMGimg  [".png", ".gif", ".tif", ".tiff", ".ppm", ".pgm", ".pbm"]
extJpg  = matchExt  IMGjpg   ".jpg"
extTxt  = matchExts IMGtxt  [".txt", ".md"]
extRaw  = matchExts IMGraw  [".nef", ".rw2"]
extDng  = matchExt  IMGdng   ".dng"
extXmp  = matchExt  IMGmeta  ".xmp"
extDxO  = matchExt  IMGdxo   ".dxo"
extPto  = matchExt  IMGhugin ".pto"
extJson = matchExt  IMGjson  ".json"

extGeo :: String -> Maybe Geo
extGeo ('.' : xs') = parseGeo' xs'
extGeo _           = mzero

parseGeo' :: String -> Maybe Geo
parseGeo' = parseMaybe pg
  where
    pg :: SP Geo
    pg = do
      x <- read <$> some digitChar
      _ <- char 'x'
      y <- read <$> some digitChar
      return (Geo x y)

-- --------------------

matchPred :: (a -> Bool) -> a -> Maybe a
matchPred p x
  | p x       = Just x
  | otherwise = Nothing

eqNoCase :: String -> String -> Bool
eqNoCase = (==) `on` map toLower

-- ----------------------------------------
