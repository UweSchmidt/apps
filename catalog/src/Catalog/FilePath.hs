{-# LANGUAGE TupleSections #-}
-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Applicative
import Data.Prim
import Text.SimpleParser

import qualified Text.SimpleParser          as SP
import qualified Text.Megaparsec.Char.Lexer as L

-- ----------------------------------------
--
-- the main entry points to file path classification
--
-- used in syncing catalog with file system

filePathToImgType :: FilePath -> NameImgType
filePathToImgType =
  fst . fpToImgType (const True) filePathConfig

filePathToExt :: ImgType -> FilePath -> Name
filePathToExt ty =
  snd . fpToImgType (== ty) filePathConfig

-- ----------------------------------------

type FnameParser = FilePath -> Maybe (NameImgType, Name)

type FilePathConfig = [FnameParser]

fpToImgType :: (ImgType -> Bool)
            -> FilePathConfig
            -> FilePath -> (NameImgType, Name)
fpToImgType tp conf path =
  fromMaybe defRes $
  foldl1 (<|>) $ map parse' conf
  where
    defRes = ((mkName path, IMGother), mempty)
    parse' p = p path >>= matchPred (tp . snd . fst)

filePathConfig :: FilePathConfig
filePathConfig =
  map (uncurry toFC) conf
  where
    toFC ty sp fp =
      toNIT <$> parseMaybe sp fp
      where
        toNIT (base, ext) = ((base, ty), ext)

    mk1 :: SP String -> SP (Name, Name)
    mk1 p = do
      x1 <- p
      return (mkName x1, mempty)

    mk2 :: SP String -> SP String -> SP (Name, Name)
    mk2 p1 p2 = do
      (x1, x2) <- nameWithSuffix p1 p2
      return (mkName x1, mkName x2)

    bn = mk2 baseName

    conf :: [(ImgType, SP (Name, Name))]
    conf =
      [ (IMGboring, mk1 boringName) -- must be 1. to filter ".", ".." and others

      , (IMGraw,    bn rawExt)
      , (IMGimg,    bn imgExt)
      , (IMGjpg,    bn jpgExt')
      , (IMGmovie,  bn movExt)
      , (IMGmeta,   bn xmpExt)
      , (IMGdxo,    bn dxoExt)
      , (IMGhugin,  bn ptoExt)
      , (IMGjson,   bn jsonExt)
      , (IMGdng,    bn dngExt)
      , (IMGtxt,    bn txtExt)
      , (IMGjpgdir, mk1 jpgdirName)
      , (IMGimgdir, mk1 imgdirName)

      -- ignore the image subdir prefix
      -- with subdir classification (>> instead of <++>)
      , (IMGcopy,   mk2 (jpgdirPre >> baseName) (geoExt <++> jpgExt'))
      , (IMGjpg,    mk2 (jpgdirPre >> baseName)              jpgExt' )
      , (IMGdng,    mk2 (jpgdirPre >> baseName)              dngExt  )
      ]

parseExt :: [String] -> SP String
parseExt = foldl1 (<|>) . map (\ s -> try $ string' s)

jpgExt'
  , rawExt, imgExt,  xmpExt, dxoExt
  , ptoExt, jsonExt, dngExt, txtExt
  , movExt :: SP String

jpgExt' = parseExt [".jpg"]
rawExt  = parseExt [".nef", ".rw2"]

rawFiles :: [String]
rawFiles = [".nef", ".rw2"]

-- sort extensions by length: ".tiff" before ".tif"
-- else backtracking with try does not work properly
imgExt  = parseExt [".png", ".tiff", ".tif", ".gif", ".ppm", ".pgm", ".pbm"]
xmpExt  = parseExt [".xmp"]
dxoExt  = parseExt ( (++) <$> (".jpg" : rawFiles)
                          <*> [".dxo", ".dop"]
                   ) -- lists are an Applicative
ptoExt  = parseExt [".pto"]
jsonExt = parseExt [".json"]
dngExt  = parseExt [".dng"]
txtExt  = parseExt [".txt", ".md"]
movExt  = parseExt [".mp4"]

geoExt :: SP String
geoExt = string "." <++> p'geo

p'geo :: SP String
p'geo = some digitChar <++> string "x" <++> some digitChar


baseName
  , imgdirName, imgdirPre
  , jpgdirName, jpgdirPre :: SP String

baseName   = some (oneOf' "-+._" <|> alphaNumChar)

imgdirName = baseName
imgdirPre  = SP.option "" (imgdirName <++> string "/")

jpgdirName =             jpgdirName' (eof >> return "")
jpgdirPre  = SP.option "" $ jpgdirName' (string "/")

jpgdirName' :: SP String -> SP String
jpgdirName' eof' =
  try ( string "srgb" <++> many digitChar <++> eof' )
  <|>
  try ( string "srgb" <++> (SP.option "" $ string "-bw") <++> og <++> eof' )
  <|>
  try ( p'geo <++> eof' )
  <|>
  try ( ( foldl1 (<|>) $
          map (\ s -> try $ string s)
          ["small", "web", "bw", "jpg", "tif", "tiff", "dng", "dxo"]
        )
        <++> og <++> eof'
      )
  where
    og :: SP String
    og = ( string "-" <|> return "" )
         <++>
         ( SP.option "" $ try
           ( some digitChar
             <++>
             ( SP.option "" $ try (string "x" <++> some digitChar) )
           )
         )

boringName :: SP String
boringName =
  (string "." <++> anyString )
  <|>
  (string "tmp" <++> anyString)
  <|>
  ( withSuffix ( string "~"
                 <|>
                 (try $ string ".bak")
                 <|>
                 (try $ string ".old")
               )
  )

-- --------------------
--
-- absolute .jpg path

jpgPath' :: SP String
jpgPath' =
  uncurry (++) <$> nameWithSuffix (string "/" <++> anyString) jpgExt'

topDir' :: SP String
topDir' = string "/" <++> some (noneOf' "/")

blazePath' :: SP (Geo, String)
blazePath' =
  (,) <$> (string "blaze-" *> geoParser) <*> anyString

fileName' :: SP String
fileName' = snd <$> anyStringThen' (some (noneOf' "/") <* eof)

-- --------------------
--
-- file path parsing combinators

nameWithSuffix :: SP String -> SP String -> SP (String, String)
nameWithSuffix np sp = do
  (n, s) <- splitSuffix sp
  case parseMaybe np n of
    Just n' -> return (n', s)
    _       -> mzero

-- ----------------------------------------

pathToBreadCrump :: String -> String
pathToBreadCrump =
  sedP (const " \8594 ") (string "/") . drop 1

-- ----------------------------------------
--
-- url pasers without regex matching

-- type SP = Parsec Void String

splitLast :: [a] -> Maybe ([a], a)
splitLast [x]      = Just ([], x)
splitLast (x : xs) = first (x:) <$> splitLast xs
splitLast []       = Nothing

joinLast :: [a] -> a -> [a]
joinLast xs x = xs ++ [x]

-- --------------------

splitAbsPath :: String -> Maybe [String]
splitAbsPath = parseMaybe pPath
  where
    pPath :: SP [String]
    pPath = do
      ps <- filter (not . null) <$> many piece
      case ps of
        [] -> mzero
        _  -> return ps

    piece = char '/' >> many (noneOf' "/")

joinAbsPath :: [String] -> String
joinAbsPath = concatMap ('/' :)

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

    part = some $ noneOf' "."
    ext  = ('.' :) <$> (char '.' >> part)

joinExt :: [String] -> String
joinExt = concat

-- splitDirFileExt "/xxx/abc.jpg" -> Just ("/xxx","abc",".jpg")
-- splitDirFileExt "/abc.jpg"     -> Just ("","abc",".jpg")
-- splitDirFileExt "/abc.txt.jpg" -> Just ("","abc.txt",".jpg")

splitDirFileExt :: String -> Maybe (String, String, String)
splitDirFileExt xs = do
  (dp, fn) <- first joinAbsPath <$> (splitAbsPath xs >>= splitLast)
  (bn, ex) <- first joinExt     <$> (splitExt     fn >>= splitLast)
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

-- used in servant server

extImg
  , extJpg
  , extTxt
  , extVideo
  , extRaw
  , extDng
  , extMeta
  , extDxO
  , extPto
  , extJson :: String -> Maybe ImgType

[ extImg
  , extJpg
  , extTxt
  , extVideo
  , extRaw
  , extDng
  , extMeta
  , extDxO
  , extPto
  , extJson
  ] = map (uncurry matchExts) imgTypeExt

imgTypeExt :: [(ImgType, [String])]
imgTypeExt =
  [ (IMGimg,   [".png", ".gif", ".tif", ".tiff", ".ppm", ".pgm", ".pbm"])
  , (IMGjpg,   [".jpg"])
  , (IMGtxt,   [".txt", ".md"])
  , (IMGmovie, [".mp4"])
  , (IMGraw,   [".nef", ".rw2"])
  , (IMGdng,   [".dng"])
  , (IMGmeta,  [".xmp"])
  , (IMGdxo,   [".dop", ".dxo"])
  , (IMGhugin, [".pto"])
  , (IMGjson,  [".json"])
  ]

fileName2ImgType :: String -> ImgType
fileName2ImgType fn =
  fromMaybe IMGother $
  foldr (<|>) mzero $
  map ($ ext) $
  map (uncurry matchExts) imgTypeExt
  where
    ext = maybe mempty last $ splitExt fn

-- --------------------

isoPicNo :: Iso' Int String
isoPicNo = iso toS frS
  where
    toS =
      ("pic-" ++ ) . reverse . take 4 . reverse . ("0000" ++ ) . show
    frS s =
      fromMaybe (-1) $ parseMaybe picNoParser s

picNoParser :: SP Int
picNoParser = string "pic-" *> L.decimal

-- --------------------

addJpg :: String -> String
addJpg fn
  | toBool (parseMaybe (withSuffix jpgExt') fn) = fn
  | otherwise                                   = fn ++ ".jpg"

-- ----------------------------------------
