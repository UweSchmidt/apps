-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Arrow (first)
import Control.Applicative
import Data.ImageTree
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Prim.Name
import Text.Regex.XMLSchema.Generic (splitSubex, Regex, parseRegexExt, matchSubexRE)

-- ----------------------------------------

filePathToImgType' :: FilePath -> NameImgType
filePathToImgType' path = fromMaybe (mkName path, IMGother) $
  parse (mk1 boringName) IMGboring
  <|>
  parse (mk1 baseName  ++ rawExt)  IMGraw
  <|>
  parse (mk1 baseName' ++ imgExt)  IMGimg
  <|>
  parse (mk1 baseName  ++ metaExt) IMGmeta
  <|>
  parse (mk1 baseName  ++ jsonExt) IMGjson
  <|>
  parse (mk1 jpgdirName) IMGjpgdir
  <|>
  parse (mk1 imgdirName) IMGimgdir
  <|>
  parse (jpgdirPre ++ mk1 baseName ++ geoExt ++ jpgExt)  IMGcopy
  <|>
  parse (jpgdirPre ++ mk1 baseName ++ jpgExt)  IMGjpg
  where
    parse re' c
      | null rest = partRes res
      | otherwise = Nothing
      where
        (res, rest) = splitSubex re' path

        partRes [("1", base)] =
          Just (mkName base, c)

        partRes _ = Nothing

    mk1  e = "({1}(" ++ e ++ "))"

    baseName  = "[-._A-Za-z0-9]+"
    baseName' = "[-._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF)"
    imgExt    = "[.](jpp|JPG|gif|tiff|ppm|pgm|pbm)"
    metaExt   = "[.](xmp|((nef|NEF|rw2|RW2|jpg|JPG)[.]dxo))"
    jsonExt   = "[.](json)"
    jpgExt    = "[.](jpg|JPG)"
    geoExt    = "[.]([0-9]+x[0-9]+)"

    imgdirName = "[-_A-Za-z0-9]+" -- no do
    jpgdirName =
      "("
      ++ ( intercalate "|"
           [ "srgb[0-9]*"
           , "srgb-bw"
           , "[0-9]+x[0-9]+"
           , "dxo"
           , "small"
           , "web"
           , "bw"
           ]
         )
      ++ ")"

    jpgdirPre =
      "(" ++ jpgdirName ++ "/)?"

    boringName = intercalate "|"
      [ "[.].*"
      , ".*~"
      , "tmp.*"
      , ".*[.](bak|old|tiff|dng)"
      ]

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
  fromMaybe ((mkName path, IMGother), emptyName) $
  foldr1 (<|>) $
  map parse conf
  where
    parse (re, ty) =
      partRes $  matchSubexRE re path
      where

        partRes [("1", base)] =
          Just ((mkName base, ty), emptyName)
        partRes [("1", base), ("2", ext)] =
          Just ((mkName base, ty), mkName ext)
        partRes _ =
          Nothing

filePathConfig :: FilePathConfig
filePathConfig = map (first parseRegexExt) $
  [ (mk1 boringName,               IMGboring)
  , (mk1 baseName  ++ mk2 rawExt,  IMGraw)
  , (mk1 baseName' ++ mk2 imgExt,  IMGimg)
  , (mk1 baseName  ++ mk2 metaExt, IMGmeta)
  , (mk1 baseName  ++ mk2 jsonExt, IMGjson)
  , (mk1 jpgdirName,               IMGjpgdir)
  , (mk1 imgdirName,               IMGimgdir)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 (geoExt ++ jpgExt),    IMGcopy)
  , (jpgdirPre
     ++ mk1 baseName
     ++ mk2 jpgExt,                IMGjpg)
  ]
  where
    mk1  e = "({1}(" ++ e ++ "))"
    mk2  e = "({2}(" ++ e ++ "))"

    baseName  = "[-._A-Za-z0-9]+"
    baseName' = "[-._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF)"
    imgExt    = "[.](jpp|JPG|gif|tiff|ppm|pgm|pbm)"
    metaExt   = "[.](xmp|((nef|NEF|rw2|RW2|jpg|JPG)[.]dxo))"
    jsonExt   = "[.](json)"
    jpgExt    = "[.](jpg|JPG)"
    geoExt    = "[.]([0-9]+x[0-9]+)"

    imgdirName = "[-_A-Za-z0-9]+" -- no do
    jpgdirName =
      "("
      ++ ( intercalate "|"
           [ "srgb[0-9]*"
           , "srgb-bw"
           , "[0-9]+x[0-9]+"
           , "dxo"
           , "small"
           , "web"
           , "bw"
           ]
         )
      ++ ")"

    jpgdirPre =
      "(" ++ jpgdirName ++ "/)?"

    boringName = intercalate "|"
      [ "[.].*"
      , ".*~"
      , "tmp.*"
      , ".*[.](bak|old|tiff|dng)"
      ]

-- ----------------------------------------
