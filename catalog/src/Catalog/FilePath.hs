-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Applicative
import Data.Prim hiding (noneOf)
import Text.Megaparsec
import Text.Megaparsec.Char

-- ----------------------------------------

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
      , ".*[.](bak|old|tiff|dng)"
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
  parseRegex ".*[.](nef|NEF||rw2|RW2|dng|DNG|jpg|JPG|xmp|XMP)"

-- ----------------------------------------

pathToBreadCrump :: String -> String
pathToBreadCrump = sed (const " \8594 ") "/" . drop 1

-- ----------------------------------------

type SP = Parsec () String

pp :: SP a -> String -> Maybe a
pp = parseMaybe

-- parse a none empty path and return the reversed path
-- so the head ist the file name, last is the top directory
--
-- fails for empty parses, e.g. for "/"

pPath :: SP [String]
pPath = do
  ps <- filter (not . null) <$> many piece
  case ps of
    [] -> mzero
    _  -> return $ reverse ps

piece :: SP String
piece = do
  char '/' >> many (noneOf "/")

-- split a file name into .-separated pieces
-- return the reversed list of parts

pExt :: SP [String]
pExt = reverse <$> sepBy1 (some $ noneOf ".") (char '.')

-- --------------------
--
-- split a path into dirPath, filename without extension and extension
--
-- pp (pPathExt (== "jpg")) "/xxx/yyy/abc.def.jpg"
--    => Just ("/xxx/yyy","abc.def","jpg")

pPathExt :: (String -> Bool) -> SP (String, String, String)
pPathExt extPred = do
  (fn : dp) <- pPath
  case parseMaybe pExt fn of
    Just (ex : n@(_ : _))
      | extPred ex
        -> return (revConcPath dp, revConcExt n, ex)
    _   -> mzero

-- split a path into dirPath, filename without last 2 extensions
-- and the last 2 extension
--
-- pp (pPathExt2 (== "jpg") (== "def")) "/xxx/yyy/abc.def.jpg"
--    => Just ("/yyy/xxx"],"abc","jpg","def")

pPathExt2 :: (String -> Bool)
          -> (String -> Bool)
          -> SP (String, String, String, String)
pPathExt2 extPred extPred2 = do
  (fn : dp) <- pPath
  case parseMaybe pExt fn of
    Just (ex : (ex2 : n@(_ : _)))
      | extPred  ex
        &&
        extPred2 ex2
        -> return (revConcPath dp, revConcExt n, ex, ex2)
    _   -> mzero


-- --------------------

revConcPath :: [String] -> String
revConcPath = foldl (\ r p -> "/" ++ p ++ r) ""

revConcExt :: [String] -> String
revConcExt [] = ""
revConcExt xs = foldl1 (\ r e -> e ++ "." ++ r) xs

-- ----------------------------------------
