{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.Convert
-- {-
  ( getImageSize
  , getThumbnailImage
  , createResizedImage
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
import           Catalog.System.ExifTool (getExifTool)
import           Data.Prim
import           Data.MetaData           (getOrientation)
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
  dst  <- toSysPath path
  dx   <- fileExist dst
  fopt <- (\ fn ->
             if null fn
             then ""
             else "-font " ++ fn
          ) <$> asks (^. envFontName . isoString)
  trc $ unwords ["genIcon", show path, show t, show dst, show dx]
  unless dx $ do
    createDir (takeDirectory <$> dst)
    verbose $ shellCmd dst fopt
    void $ execProcess "bash" [] (shellCmd dst fopt)
  where
    shellCmd :: SysPath -> String -> String
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
      , dst ^. isoFilePath
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
        (s2, s3)  = splitAt len3 r2

-- ----------------------------------------

getThumbnailImage :: FilePath -> FilePath -> Cmd (Maybe FilePath)
getThumbnailImage src dst = do
  sp <- toSysPath src
  dp <- toSysPath dst
  ( extractImage sp dp >> return (Just dst) )
    `catchError`
    ( \ e ->
        do warn $
             unwords [ "getThumbnailImage: no Thumbnail found in "
                     , show sp ++ ","
                     , "reason:"
                     , show e
                     ]
           return Nothing
    )
  where
    extractImage :: SysPath -> SysPath -> Cmd ()
    extractImage sp dp = do
      _ <- execProcess "bash" [] $
           unwords [ "exiftool"
                   , "'-b'"
                   , "'-ThumbnailImage'"
                   , "'" ++ sp ^. isoFilePath ++ "'"
                   , ">"
                   , "'" ++ dp ^. isoFilePath ++ "'"
                   ]
      unlessM (fileNotEmpty dp) $
        abort $ "empty thumbnail file " ++ show dp
      return ()

-- ----------------------------------------

getImageSize    :: FilePath -> Cmd Geo
getImageSize fp =
  (fromMaybe geo'org . readGeo'') <$>
  (toSysPath fp >>= execImageSize)
  where
    execImageSize :: SysPath -> Cmd String
    execImageSize sp
      = execProcess "exiftool" ["-s", "-ImageSize", sp ^. isoFilePath] ""
        `catchError`
        ( const $ do
            warn $ "getImageSize: size couldn't be determined for image " ++
                   show sp
            return ""
        )

scaleWidth :: Int -> Geo -> Geo
scaleWidth h' (Geo w h) = Geo w' h'
  where
    w' = (w * h' + (h - 1)) `div` h

-- ----------------------------------------

createResizedImage :: GeoAR -> FilePath -> FilePath -> Cmd ()
createResizedImage d'geo src dst = do
  sp    <- toSysPath src
  dp    <- toSysPath dst
  ori   <- getOrientation <$> getExifTool sp
  s'geo <- getImageSize src
  createResized ori s'geo sp dp
  where
    createResized rot s'geo sp dp
      -- resize is a noop so a link is sufficient
      | "#" `isPrefixOf` shellCmd = do
          trc "createResizedImage: make link to src"
          linkFile sp dp
      -- resize done with external prog convert
      | otherwise = do
          trc $ "createResizedImage: " ++ show shellCmd
          runDry ("create image copy: " ++ show shellCmd) $
            execProcess "bash" [] shellCmd >> return ()
      where
        shellCmd =
          buildCmd rot d'geo s'geo dp sp

buildCmd :: Int -> GeoAR -> Geo -> SysPath -> SysPath -> String
buildCmd rot d'g s'geo d s =
  buildCmd2 rot d'g' s'geo d s
  where
    d'geo = d'g ^. theGeo
    d'g'
      | d'geo == geo'org = d'g & theGeo .~ s'geo  -- orgiginal size demaned
                               & theAR  .~ Pad
      | otherwise        = d'g

buildCmd2 :: Int -> GeoAR -> Geo -> SysPath -> SysPath -> String
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

buildCmd3 :: [String] -> GeoAR -> Geo -> SysPath -> SysPath -> String
buildCmd3 rotate d'g s'geo d' s'
  | d'geo == s'geo
    &&
    null rotate
    &&
    ".jpg" `isSuffixOf` s = "# nothing to do for " ++ show s
  | otherwise = shellCmd
  where
    s           = s' ^. isoFilePath
    d           = d' ^. isoFilePath
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
  sp  <- toSysPath src
  dx  <- fileExist sp
  trc $ unwords ["genBlogText", show src, show dx]
  if dx
    then readFileT sp
    else return $ ("no file found for blog text: " ++ show src) ^. isoText

genBlogHtml :: FilePath -> Cmd Text
genBlogHtml src = do
  sp  <- toSysPath src
  dx  <- fileExist sp
  trc $ unwords ["genBlogText", show src, show dx]
  if dx
    then formatBlogText sp
    else return $ ("no file found for blog text: " ++ show src) ^. isoText

formatBlogText :: SysPath -> Cmd Text
formatBlogText sp =
  (^. isoText) <$>
    execProcess "pandoc" ["-f", "markdown", "-t", "html", sp ^. isoFilePath] ""

writeBlogText :: Text -> FilePath -> Cmd ()
writeBlogText t dst = do
  dp <- toSysPath dst
  writeFileT dp t

-- ----------------------------------------
