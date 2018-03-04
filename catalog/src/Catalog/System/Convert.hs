{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.Convert
-- {-
  ( getImageSize
  , getColImgSize
  , createImageCopy
  , genImageFrom
  , genImage
  , createImageFromTxt
  , genIcon
  , genAssetIcon
  , genBlogText
  , genBlogHtml
  , writeBlogText
  , selectFont
  , scaleWidth
  )
-- -}
where

import           Catalog.Cmd
import           Catalog.FilePath
import           Catalog.System.ExifTool (getExifTool)
import           Data.Prim
import           Data.ImgNode
import           Data.MetaData
import qualified Data.Text as T
import           Text.SimpleParser

-- import Debug.Trace

-- ----------------------------------------

genAssetIcon :: String -> String -> Cmd (Maybe FilePath)
genAssetIcon px s = do
  trc $ "genAssetIcon: " ++ show f ++ " " ++ show s
  genIcon f s   -- call convert with string s, please no "/"-es in s
  return $ Just f
  where
    f = ps'iconsgen </> px ++ ".jpg"

genIcon :: FilePath -> String -> Cmd ()
genIcon path t = do
  dst  <- (++ path) <$> view envMountPath
  dx   <- fileExist dst
  fopt <- (\ fn ->
            if null fn
            then ""
            else "-font " ++ fn
           ) <$> asks (^. envFontName . isoString)
  trc $ unwords ["genIcon", show path, show t, show dst, show dx]
  unless dx $ do
    createDir $ takeDirectory dst
    trc  $ shellCmd dst fopt
    void $ execProcess "bash" [] (shellCmd dst fopt)
  where
    shellCmd dst fopt =
      unwords $
      [ "convert"
      , "-background 'rgb(255,255,255)'"
      , "-fill 'rgb(192,64,64)'"
      ]
      ++
      [ fopt ]
      ++
      [ "-size 600x400"
      , "-pointsize " ++ ps'
      , "-gravity center"
      , "label:'" ++ t' ++ "'"
      , "-background 'rgb(128,128,128)'"
      , "-vignette 0x40"
      , dst
      ]
    (t', ps')
      | multiline = (t0, ps0)
      | len <= 10 = (t, "92")
      | len <= 20 = (t1 ++ "\\n" ++ t2, "80")
      | otherwise = (s1 ++ "\\n" ++ s2 ++ "\\n" ++ s3, "60")
      where
        ls        = lines t
        lsn       = length ls
        multiline = lsn > 1
        t0        = intercalate "\\n" ls
        ps0
          | lsn == 2  = "80"
          | lsn == 3  = "60"
          | lsn == 4  = "50"
          | otherwise = "40"
        len       = length t
        len2      = len `div` 2
        len3      = len `div` 3
        (t1, t2)  = splitAt len2 t
        (s1, r2)  = splitAt len3 t
        (s2, s3) = splitAt len3 r2

-- ----------------------------------------

genImage :: GeoAR -> ImgRef -> Cmd FilePath
genImage geo (ImgRef oid nm) = do
  n       <- getImgVal oid
  let ityp = fromMaybe IMGother $
             n ^? theParts . isoImgPartsMap . ix nm . theImgType
  srcPath <- buildImgPath (ImgRef oid nm)
  genImageFrom ityp geo srcPath (addJpg srcPath)


genImageFrom :: ImgType -> GeoAR -> String -> String -> Cmd FilePath

genImageFrom IMGjpg geo src path =
  fillCache geo src path createImageCopy

genImageFrom IMGimg geo src path =
  fillCache geo src path createImageCopy

genImageFrom IMGtxt geo src path =
  fillCache geo src path createImageFromTxt

genImageFrom srcType _g  _s path =
  abort $
  unwords [ "genImageFrom: unsupported media type"
          ,  show srcType
          , "for"
          , show path
          ]

-- ----------------------------------------

fillCache :: GeoAR -> FilePath -> FilePath
      -> (GeoAR -> FilePath -> FilePath -> Cmd FilePath)
      -> Cmd FilePath
fillCache geo srcPath path genImg = do
  mp <- view envMountPath
  let src = mp ++ srcPath
  let dst = mp ++ ps'cache </> (geo ^. isoString) ++ path
  sx <- fileExist src
  if sx
    then do
    dx <- fileExist dst
    dw <- if dx
          then getModiTime dst
          else return mempty
    sw <- getModiTime src
    if dw <= sw
      then
      ( do
          createDir $ takeDirectory dst
          genImg geo dst src
      )
      `catchError`      -- if the org image is broken
      ( const $ do      -- a "broken image" icon is generated
          warn $ "image couldn't be converted or resized: " ++ show path
          broken <-
            fromMaybe ps'blank <$>
            genAssetIcon "brokenImage" "broken\nimage"
          warn $ "generate a substitute: " ++ show broken
          fillCache geo broken broken createImageCopy
      )
      else
      return dst
    else
    notThere path

notThere :: FilePath -> Cmd FilePath
notThere url =
  runDry msg (abort msg) >> return url
  where
    msg = "image not found: " ++ url

-- ----------------------------------------

getImageSize    :: FilePath -> Cmd Geo
getImageSize f =
  (fromMaybe geo'org . readGeo'') <$> execImageSize
  where
    execImageSize :: Cmd String
    execImageSize
      = execProcess "exiftool" ["-s", "-ImageSize", f] ""
        `catchError`
        ( const $ do
            warn $ "getImageSize: size couldn't be determined for image " ++
                   show f
            return ""
        )

-- get the geo from an absolut path in catalog
--
-- imgRef:   /archive/photos/pic-30.jpg
-- fs path:  "mountPath/photos/pic-30.jpg"

getColImgSize :: FilePath -> Cmd Geo
getColImgSize imgRef = do
  mp <- view envMountPath
  getImageSize $ mp ++ drop (length ps'archive) imgRef

scaleWidth :: Int -> Geo -> Geo
scaleWidth h' (Geo w h) = Geo w' h'
  where
    w' = (w * h' + (h - 1)) `div` h

-- ----------------------------------------

-- try to create an icon from some text found in the file path
-- if s is a .md or .txt file, take the 1. line of that file contents

createImageFromTxt :: GeoAR -> FilePath -> FilePath -> Cmd FilePath
createImageFromTxt d'geo d s =
  go
  where
    go = do
      headline <-
        T.concat . take 1 . filter (not . T.null) . map cleanup . T.lines <$>
        readFileT s
      let str1 = headline ^. isoString
      let str2 = sedP (const "") (many (noneOf' "/") >> char '/') s
      let str  = concat . take 1 . filter (not . null) $ [str1, str2]
      trc $ unwords ["createImageFromTxt:", show d'geo, d, s, str]
      icon <-
        fromMaybe ps'blank <$>
        genAssetIcon (mapBad $ dropWhile (== '.') s) str
      genImageFrom IMGjpg d'geo icon icon

    mapBad :: String -> String
    mapBad = sedP (const "_") (oneOf' " /$")

    cleanup :: Text -> Text
    cleanup = T.dropWhile (not . isAlphaNum)

-- ----------------------------------------

-- rot == 0:  no rotate
-- rot == 1:  90 degrees clockwise
-- rot == 2: 180 degrees clockwise
-- rot == 3: 270 degrees clockwise

createImageCopy :: GeoAR -> FilePath -> FilePath -> Cmd FilePath
createImageCopy d'geo d s = do
  ori <- getOrientation <$> getExifTool s
  createImageCopy' ori d'geo d s

createImageCopy' :: Int -> GeoAR -> FilePath -> FilePath -> Cmd FilePath
createImageCopy' rot d'geo d s =
  getImageSize s >>= go
  where
    go s'geo
      | "#" `isPrefixOf` shellCmd = do
          trc $ "createImageCopy: " ++ shellCmd
          return s
      | otherwise = do
          runDry ("create image copy: " ++ show shellCmd) $
            execProcess "bash" [] shellCmd >> return ()
          return d
      where
        shellCmd = buildCmd rot d'geo s'geo d s

buildCmd :: Int -> GeoAR -> Geo -> FilePath -> FilePath -> String
buildCmd rot d'g s'geo d s =
  buildCmd2 rot d'g' s'geo d s
  where
    d'geo = d'g ^. theGeo
    d'g'
      | d'geo == geo'org = d'g & theGeo .~ s'geo  -- orgiginal size demaned
                               & theAR  .~ Pad
      | otherwise        = d'g

buildCmd2 :: Int -> GeoAR -> Geo -> FilePath -> FilePath -> String
buildCmd2 rot d'g s'geo d s =
  buildCmd3 os d'g s'geo' d s
  where
    os
      | rot' /= 0 = ["-rotate", show (rot' * 90)]
      | otherwise = []

    s'geo'
      | odd rot   = flipGeo s'geo
      | otherwise =         s'geo

    rot' = rot `mod` 4

buildCmd3 :: [String] -> GeoAR -> Geo -> FilePath -> FilePath -> String
buildCmd3 rotate d'g s'geo d s
  | d'geo == s'geo
    &&
    null rotate
    &&
    ".jpg" `isSuffixOf` s = "# nothing to do for " ++ show s
  | otherwise = shellCmd
  where
    d'geo       = d'g ^. theGeo
    aspect      = d'g ^. theAR
    (Geo cw ch, Geo xoff yoff)
                = crGeo aspect
    r           = rGeo  aspect

    rGeo Fix    = d'geo
    rGeo _      = resizeGeo s'geo d'geo

    crGeo Fix   = cropGeo s'geo d'geo
    crGeo Pad   = (s'geo, Geo (-1) (-1))
    crGeo Crop  = (s'geo, Geo 0 0)

    resize      = ["-thumbnail", geo ++ "!"]
    resize1     = ( if isThumbnail
                    then "-thumbnail"
                    else "-geometry"
                  ) : [d'geo ^. isoString]
    quality     = "-quality" : [ if isThumbnail
                                 then "75"
                                 else "90"
                               ]
    interlace   = [ "-interlace", "Plane" ]

    isPad       = (xoff == (-1) && yoff == (-1))
    isCrop      = (xoff > 0     || yoff > 0)
    isThumbnail = d'geo ^. theW <= 300 && d'geo ^. theH <= 300
    geo         = r ^. isoString

    cmdName
        | isPad         = [ "convert" ] -- ["montage" ]
        | otherwise     = [ "convert" ]

    cmdArgs
        | isPad         = resize1
                          ++ [ "-background", "'#333333'" ]
                       -- ++ [ "-size", show (2*w) ++ "x" ++ show (2*h) ]
                       -- this gives too low quality
                          ++ [ s, d ]
        | isCrop        = [ "-crop", show cw ++ "x" ++ show ch ++ "+" ++
                            show xoff ++ "+" ++ show yoff
                          , s, "miff:-"
                          , "|"
                          , "convert"
                          ]
                          ++ resize ++ quality
                          ++ ["miff:-", d ]
        | otherwise     = resize ++ [s, d]

    shellCmd    = unwords $
                  cmdName
                  ++ interlace
                  ++ quality
                  ++ rotate
                  ++ cmdArgs

-- ----------------------------------------

resizeGeo       :: Geo -> Geo -> Geo
resizeGeo sGeo@(Geo sw sh) (Geo dw dh)
    | sw <= dw && sh <= dh              -- source fits into display
        = sGeo                          -- no downsizing, no magnification

    | sw * dh >= dw * sh                -- source wider than display
        = Geo dw (dw * sh `div` sw)     -- maximum width, height scaled down

    | otherwise                         -- source higher than display
        = Geo (dh * sw `div` sh) dh     -- maximum height, width scaled down


cropGeo         :: Geo -> Geo -> (Geo, Geo)
cropGeo (Geo sw sh) (Geo dw dh)
    | sw *dh >= dw * sh                 -- source wider than reqired
        = (Geo sw' sh, Geo xoff 0)
    | otherwise                         -- sorce highter than required
        = (Geo sw sh', Geo 0 yoff)
    where
    sw'  = dw * sh `div` dh
    xoff = (sw - sw') `div` 2           -- cut off left and right parts
    sh'  = dh * sw `div` dw
    yoff = (sh - sh') `div` 3           -- cut off 1/3 from top and 2/3 from bottom
                                        -- else important parts like heads
                                        -- are cut off (Ouch!!)

-- ----------------------------------------

selectFont :: Cmd Text
selectFont =
  (sel <$> fontList)
  `catchError`
  (const $ return mempty)
  where
    sel flist = head $ filter (`elem` flist) fs ++ mempty
    fs = ["ComicSans", "Helvetica"]

fontList :: Cmd [Text]
fontList = toFL <$> execProcess "bash" [] shellCmd
  where
    shellCmd =
      "convert -list font | grep Font: | sed 's|^.*Font: ||'"

    toFL :: String -> [Text]
    toFL = map (^. isoText) . lines

-- ----------------------------------------

genBlogText :: FilePath -> Cmd Text
genBlogText src = do
  dx  <- fileExist src
  trc $ unwords ["genBlogText", show src, show dx]
  if dx
    then readFileT src
    else return $ ("no file found for blog text: " ++ show src) ^. isoText

genBlogHtml :: FilePath -> Cmd Text
genBlogHtml src = do
  dx  <- fileExist src
  trc $ unwords ["genBlogText", show src, show dx]
  if dx
    then formatBlogText src
    else return $ ("no file found for blog text: " ++ show src) ^. isoText

formatBlogText :: FilePath -> Cmd Text
formatBlogText f =
  (^. isoText) <$>
    execProcess "pandoc" ["-f", "markdown", "-t", "html", f] ""

writeBlogText :: Text -> FilePath -> Cmd ()
writeBlogText t dst = do
  writeFileT dst t

-- ----------------------------------------
