module PPL.Picture
    ( Picture
    , aboveMx
    , bitmapPic
    , blackAndWhitePic
    , concatHMx
    , concatVMx
    , cutMx
    , diffPic
    , flipDMx
    , flipHMx
    , flipVMx
    , gammaPic
    , greyPic
    , heightMx
    , invDiffPic
    , invertPic
    , invMeanPic
    , maxPic
    , meanPic
    , mergeHMx
    , mergeVMx
    , minPic
    , mulPic
    , partHMx
    , partVMx
    , pasteMx
    , readPictureFile
    , reduceColorPic
    , replicateMx
    , resizePic
    , rotateMx
    , scaleMx
    , shiftMx
    , shrinkMx
    , sideBySideMx
    , splitHMx
    , splitVMx
    , widthMx
    , writePictureFile
    )
where

-- ------------------------------

{-
import           Matrix.Picture hiding (mergeHMx, mergeVMx)
import qualified Matrix.Picture as M
import           Matrix.Ppm

import System.IO

mergeHMx, mergeVMx      :: Picture -> Picture -> Picture

mergeHMx i1 i2  = M.mergeHMx [i1,i2]
mergeVMx i1 i2  = M.mergeVMx [i1,i2]

shrinkMx                :: Int -> Int -> Picture -> Picture
shrinkMx                = resizePic

readPictureFile         :: String -> IO Picture
readPictureFile f
    = do
      c <- readFile s
      return (readPicture c)

writePictureFile        :: String -> Picture -> IO ()
writePictureFile f p
    = do
      f' <- openFile s WriteMode
      hPutStr f' (formatPGMbin p)
      hClose f'
    where
    f' = replaceExtension f ".pgm"

formatPBM       = showPNM    . pictureToPBM
formatPGM       = showPNM    . pictureToPGM
formatPGMbin    = showPNMbin . pictureToPGM

-}

-- ------------------------------

import ImgFct.Image

type Picture    = PixMap

-- todo :: a -> a
-- todo = id

aboveMx                 :: Picture -> Picture -> Picture
aboveMx                 = above

bitmapPic               :: Picture -> Picture
bitmapPic               = bitmap1

blackAndWhitePic        :: Picture -> Picture
blackAndWhitePic        = bitmap

concatHMx               :: [Picture] -> Picture
concatHMx               = sideBySides

concatVMx               :: [Picture] -> Picture
concatVMx               = aboves

cutMx                   :: Int -> Int -> Int -> Int -> Picture -> Picture
cutMx x y w h           = cut (mkGeo w h) . shift (0 - x) (0 - y)

diffPic                 :: Picture -> Picture -> Picture
diffPic                 = melt (\ x y -> (x - y + 1) / 2)

flipDMx                 :: Picture -> Picture
flipDMx                 = flipD

flipHMx                 :: Picture -> Picture
flipHMx                 = flipH

flipVMx                 :: Picture -> Picture
flipVMx                 = flipV

gammaPic                :: Double -> Picture -> Picture
gammaPic                = gamma

greyPic                 :: Double -> Int -> Int -> Picture
greyPic l w h           = mkGrey (mkGeo w h) (uniChannel l)

heightMx                :: Picture -> Int
heightMx (Image (Geo _w h) _i)
                        = h

invDiffPic              :: Picture -> Picture -> Picture
invDiffPic              = melt $ (min 1 . max 0) `on2` (\ x y -> x + y - 0.5)

invertPic               :: Picture -> Picture
invertPic               = invert

invMeanPic              :: Picture -> Picture -> Picture
invMeanPic              = melt $ (min 1 . max 0) `on2` (\ x y -> x - y + 0.5)

maxPic                  :: Picture -> Picture -> Picture
maxPic                  = melt max

meanPic                 :: Picture -> Picture -> Picture
meanPic                 = melt (\ x y -> (x + y) / 2)

mergeHMx                :: Picture -> Picture -> Picture
mergeHMx                = mergeY

mergeVMx                :: Picture -> Picture -> Picture
mergeVMx                = mergeX

minPic                  :: Picture -> Picture -> Picture
minPic                  = melt min

mulPic                  :: Picture -> Picture -> Picture
mulPic                  = melt (*)

partHMx                 :: Int -> Picture -> [Picture]
partHMx                 = partY

partVMx                 :: Int -> Picture -> [Picture]
partVMx                 = partX

pasteMx                 :: Int -> Int -> Picture -> Picture -> Picture
pasteMx x y p1 p2       = shift x y (addMask p1) `overlay` p2

readPictureFile         :: String -> IO Picture
readPictureFile         = readImageFile

reduceColorPic          :: Int -> Picture -> Picture
reduceColorPic          = reduce

replicateMx             :: Int -> Int -> Picture -> Picture
replicateMx             = tile

resizePic               :: Int -> Int -> Picture -> Picture
resizePic n m           = resizeY m . resizeX n

rotateMx                :: Picture -> Picture
rotateMx                = rot180

scaleMx                 :: Int -> Int -> Picture -> Picture
scaleMx                 = scale

shiftMx                 :: Int -> Int -> Picture -> Picture
shiftMx                 = shiftRot

shrinkMx                :: Int -> Int -> Picture -> Picture
shrinkMx                = shrink

sideBySideMx            :: Picture -> Picture -> Picture
sideBySideMx            = sideBySide

splitHMx                :: Int -> Picture -> [Picture]
splitHMx                = splitY

splitVMx                :: Int -> Picture -> [Picture]
splitVMx                = splitX

widthMx                 :: Picture -> Int
widthMx (Image (Geo w _h) _i)
                        = w

writePictureFile        :: String -> Picture -> IO ()
writePictureFile        = writeImageFile


