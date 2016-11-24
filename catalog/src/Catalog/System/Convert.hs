{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.Convert
{- }
  ( getImageSize
       , createImageCopy
       , genImage
       , createImageFromTxt
       , genImageFromTxt
       , genIcon
       , genAssetIcon
       , genBlogText
       , genBlogHtml
       , writeBlogText
       , selectFont
       ) -}
where

import           Catalog.Cmd
import           Catalog.FilePath
import           Data.Prim
import qualified Data.Text as T
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

genImageFromTxt :: FilePath -> Cmd FilePath
genImageFromTxt =
  genImage' createImageFromTxt (objPath2Geo txtPathExpr) (objSrc txtSrcExpr)

-- ----------------------------------------

-- generate an image of a given geometry from an original image
-- from the archive or assets
-- geometry is specified by the the first part of the url2
--
-- example:             "/fix-160x120/archive/photo/pic-30.jpg"
--
-- dst img is           "mountPath/fix-160x120/photo/pic-30.jpg"
-- org img is           "mountPath/photo/pic-30.jpg"

genImage :: FilePath -> Cmd FilePath
genImage = genImage' (createImageCopy 0) (objPath2Geo imgPathExpr) (objSrc imgSrcExpr)

genImage' :: (GeoAR -> FilePath -> FilePath -> Cmd FilePath) ->
             (FilePath -> Maybe (GeoAR, FilePath)) ->
             (FilePath -> FilePath) ->
             FilePath -> Cmd FilePath
genImage' createImageC imgPath2Geo imgSrc url = do
  maybe notThere (doit createImageC) $ imgPath2Geo url
    where
      notThere =
        abort $ "image not found: " ++ show url

      doit :: (GeoAR -> FilePath -> FilePath -> Cmd FilePath) ->
              (GeoAR, FilePath) -> Cmd FilePath
      doit createImg (geo, path) = do
        mp <- view envMountPath
        let src = mp ++ imgSrc path
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
                    createImg geo dst src
                  )
                  `catchError`      -- if the org image is broken
                  ( const $ do      -- a "broken image" icon is generated
                      warn $ "image couldn't be converted or resized: " ++ show url
                      broken <-
                        fromMaybe ps'blank <$>
                        genAssetIcon "brokenImage" "broken\nimage"
                      warn $ "generate a substitute: " ++ show broken
                      doit (createImageCopy 0) (geo, broken)
                  )
              else
                return dst
          else
            notThere

-- ----------------------------------------

objPath2Geo :: Regex -> FilePath -> Maybe (GeoAR, FilePath)
objPath2Geo iex p =
  case matchSubexRE iex p of
    [("geoar", geoar), ("topdir", topdir), ("path", path)]
      | topdir == p'archive ^. isoString ->
          -- remove the redundant "/archive" part for images from the archive
          Just (geoar ^. from isoString, path)

      | otherwise ->
          -- remain the top dir part, e.g for "/assets/icons/blank.jpg""
          Just (geoar ^. from isoString, topdir ++ path)

    _ ->  -- wrong url
          Nothing

-- ----------------------------------------

getImageSize    :: FilePath -> Cmd Geo
getImageSize f =
  execImageSize f >>= parseGeo

execImageSize :: FilePath -> Cmd String
execImageSize f
  = execProcess "exiftool" ["-s", "-ImageSize", f] ""

parseGeo :: String -> Cmd Geo
parseGeo s =
  maybe (abort $ "parseGeo: no parse: " ++ s) return $
  readGeo' s

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
      let str2 = sed (const "") ".*/" s  -- base name -- pathToBreadCrump s
      let str  = concat . take 1 . filter (not . null) $ [str1, str2]
      trc $ unwords ["createImageFromTxt:", show d'geo, d, s, str]
      icon <-
        fromMaybe ps'blank <$>
        genAssetIcon (sed (const "_") "[ /$]" s) str
      genImage $ "/" ++ d'geo ^. isoString ++ icon

    cleanup :: Text -> Text
    cleanup = T.dropWhile (not . isAlphaNum)

-- ----------------------------------------

-- rot == 0:  no rotate
-- rot == 1:  90 degrees clockwise
-- rot == 2: 180 degrees clockwise
-- rot == 3: 270 degrees clockwise

createImageCopy :: Int -> GeoAR -> FilePath -> FilePath -> Cmd FilePath
createImageCopy rot d'geo d s =
  getImageSize s >>= go
  where
    go s'geo = do
      runDry ("create image copy: " ++ show shellCmd) $ do
        execProcess "bash" [] shellCmd >> return ()
      return d
      where
        shellCmd = resizeShellCmd rot d'geo s'geo d s

resizeShellCmd :: Int -> GeoAR -> Geo -> FilePath -> FilePath -> String
resizeShellCmd rot d'g s'geo0 d s =
  shellCmd
  where
    s'geo
      | even rot  =         s'geo0
      | otherwise = flipGeo s'geo0

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

    unsharp     = [] -- ["-unsharp", "0.7x0.7+1.0+0.05"] -- sharpen option removed
    resize      = ["-thumbnail", geo ++ "!"]
    -- resize1     = ["-geometry", geo, "-thumbnail", geo]
    resize1     = ( if isThumbnail
                    then "-thumbnail"
                    else "-geometry"
                  ) : [d'geo ^. isoString]
    quality     = "-quality" : [ if isThumbnail
                                 then "75"
                                 else "95"
                               ]
    interlace   = [ "-interlace", "Plane" ]

    rotate
      | rot == 0  = []
      | otherwise = [ "-rotate", show ((rot `mod` 4) * 90) ]

    isPad       = (xoff == (-1) && yoff == (-1))
    isCrop      = (xoff > 0     || yoff > 0)
    isThumbnail = let Geo dw dh = d'geo
                  in dw <= 300 && dh <= 300
    geo         = r ^. isoString

    cmdName
        | isPad         = [ "convert" ] -- ["montage" ]
        | otherwise     = [ "convert" ]

    cmdArgs
        | isPad         = resize1
                          ++ [ "-background", "'#333333'" ]
                       -- ++ [ "-size", show (2*w) ++ "x" ++ show (2*h) ] -- this gives too low quality
                          ++ [ s, d ]
        | isCrop        = [ "-crop", show cw ++ "x" ++ show ch ++ "+" ++ show xoff ++ "+" ++ show yoff
                          , s, "miff:-"
                          , "|"
                          , "convert"
                          ]
                          ++ resize ++ unsharp ++ quality
                          ++ ["miff:-", d ]
        | otherwise     = resize ++ unsharp ++ [s, d]

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
                                        -- else important parts like heads are cut off (Ouch!!)

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
      "convert -list font| grep Font: | sed 's|^.*Font: ||'"

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
