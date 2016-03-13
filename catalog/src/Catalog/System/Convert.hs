module Catalog.System.Convert
       ( getImageSize
       , createImageCopy
       )
where

import Catalog.Cmd
import Data.Prim

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

createImageCopy :: GeoAR -> FilePath -> FilePath -> Cmd ()
createImageCopy d'geo d s =
  getImageSize s >>= go
  where
    go s'geo =
      runDry ("create image copy: " ++ show shellCmd) $ do
        execProcess "bash" [] shellCmd >> return ()
      where
        shellCmd = resizeShellCmd d'geo s'geo d s

resizeShellCmd :: GeoAR -> Geo -> FilePath -> FilePath -> String
resizeShellCmd d'g s'geo d s =
  shellCmd
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

    unsharp     = [] -- ["-unsharp", "0.7x0.7+1.0+0.05"] -- sharpen option removed
    resize      = ["-thumbnail", geo ++ "!"]
    resize1     = ["-geometry", geo, "-thumbnail", geo]
    quality     = ["-quality", "85"]
    interlace   = [ "-interlace", "Plane" ]
    isPad       = (xoff == (-1) && yoff == (-1))
    isCrop      = (xoff > 0     || yoff > 0)
    geo         = r ^. isoString

    cmdName
        | isPad         = [ "montage" ]
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
