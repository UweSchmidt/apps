-- | classify file names and compute a file type for a file name/path

module Catalog.FilePath where

import Control.Applicative
import Data.Prim

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
  map parse conf
  where
    parse (e, ty) =
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
  , (mk1 baseName  ++ mk2 jpgExt,  IMGjpg)
  , (mk1 baseName  ++ mk2 imgExt,  IMGimg)
  , (mk1 baseName  ++ mk2 xmpExt,  IMGmeta)
  , (mk1 baseName  ++ mk2 jsonExt, IMGjson)
  , (mk1 baseName  ++ mk2 ptoExt,  IMGhugin)
  , (mk1 baseName  ++ mk2 dxoExt,  IMGdxo)
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

    baseName  = "[-+._A-Za-z0-9]+"
    rawExt    = "[.](nef|NEF||rw2|RW2)"
    imgExt    = "[.](gif|tiff?|png|ppm|pgm|pbm)"
    xmpExt    = "[.](xmp|XMP)"
    dxoExt    = "[.]((nef|NEF|rw2|RW2|jpg|JPG)[.]dop)"
    ptoExt    = "[.]pto"
    jsonExt   = "[.](json)"
    jpgExt    = "[.](jpg|JPG)"
    geoExt    = "[.]([0-9]+x[0-9]+)"

    imgdirName = "[-+._A-Za-z0-9]+" -- no do
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
           , "jpg"
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
