-- $Id: Picture.hs,v 1.13 2001/12/11 22:30:14 uwe Exp $

module Matrix.Picture
    ( Picture
    , module Matrix.Matrix

    , blackPic
    , whitePic
    , greyPic

    , gammaPic
    , invertPic
    , blackAndWhitePic
    , reduceColorPic
    , bitmapPic
    , resizePic

    , meanPic
    , diffPic
    , invMeanPic
    , invDiffPic
    , mulPic
    , minPic
    , maxPic

    ) where

import Matrix.Matrix
import Data.List(mapAccumL)

type Picture    = Matrix Double

blackPic        :: Int -> Int -> Picture
blackPic        = greyPic 0.0

whitePic        :: Int -> Int -> Picture
whitePic        = greyPic 1.0

greyPic         :: Double -> Int -> Int -> Picture
greyPic light w h
    = replicate (max h 0) (replicate (max w 0) (toColor light))


gammaPic        :: Double -> Picture -> Picture
gammaPic g      = mapMx (gamma g)

invertPic       :: Picture -> Picture
invertPic       = mapMx (\ x -> 1.0 - x)

bitmapPic       :: Picture -> Picture
bitmapPic       = snd . sumUpCol 0.5

blackAndWhitePic:: Picture -> Picture
blackAndWhitePic
    = mapMx (\ x -> if x > 0.5 then 1.0 else 0.0)


reduceColorPic  :: Int -> Picture -> Picture
reduceColorPic n
    = mapMx (redColor (n `max` 2))

resizePic       :: Int -> Int -> Picture -> Picture
resizePic w h
    | w <= 0 || h <= 0
        = zeroMx
    | otherwise
        = resizeHeight h . resizeWidth w

meanPic         :: Picture -> Picture -> Picture
meanPic         = zipWithMx arithmeticMean

diffPic         :: Picture -> Picture -> Picture
diffPic         = zipWithMx diff

mulPic          :: Picture -> Picture -> Picture
mulPic          = zipWithMx (*)

minPic          :: Picture -> Picture -> Picture
minPic          = zipWithMx min

maxPic          :: Picture -> Picture -> Picture
maxPic          = zipWithMx max

invDiffPic      :: Picture -> Picture -> Picture
invDiffPic      = zipWithMx invDiff

invMeanPic      :: Picture -> Picture -> Picture
invMeanPic      = zipWithMx invArithmMean

-- -------------------------------------------------------------------

-- local functions

int2float       :: Int -> Double
int2float       = fromInteger . toInteger

redColor        :: Int -> Double -> Double
redColor n c
    = toColor (int2float (floor (c * n')) / n1')
      where
      n'  = int2float n
      n1' = n' - 1.0

toColor         :: Double -> Double
toColor         = min 1.0 . max 0.0

gamma           :: Double -> Double -> Double
gamma g x
    | x == 0.0  = 0.0
    | otherwise = x ** recip g

arithmeticMean  :: Double -> Double -> Double
arithmeticMean x y
    = (x + y) / 2.0

diff            :: Double -> Double -> Double
diff x y
    = (x - y + 1.0) / 2.0

invArithmMean   :: Double -> Double -> Double
invArithmMean x y
    = toColor (x - y + 0.5)

invDiff         :: Double -> Double -> Double
invDiff x y
    = toColor (x + y - 0.5)

sumUp           :: Double -> Double -> (Double, Double)
sumUp s x
    | s' >= 1.0 = (s' - 1.0, 1.0)
    | otherwise = (s', 0.0)
    where
    s' = s + x

sumUpRow        :: Double -> [Double] -> (Double, [Double])
sumUpRow        = mapAccumL sumUp

sumUpCol        :: Double -> Picture -> (Double, Picture)
sumUpCol        = mapAccumL sumUpRow

wSum            :: Int -> Int -> (Int, Double) -> Double -> ((Int, Double), [Double])
wSum ln la (w, s) x
    | w + ln < la
        = ((w + ln, s + int2float ln * x), [])
    | otherwise
        = ((w2', int2float w2' * x), x1 : replicate n x)
          where
          w1  = la - w
          w2  = ln - w1
          n   = w2 `div` la
          w2' = w2 - la * n
          x1  = (s + int2float w1 * x) / int2float la

wSums           :: Int -> Int -> (Int, [Double]) -> [Double] -> ((Int, [Double]), [[Double]])
wSums ln la (w, ss) xs
    | w + ln < la
        = ((w + ln, zipWith (\ s x -> s + int2float ln * x) ss xs), [])
    | otherwise
        = ((w2', map (\ x -> int2float w2' * x) xs), xs1 : replicate n xs)
          where
          w1  = la - w
          w2  = ln - w1
          n   = w2 `div` la
          w2' = w2 - la * n
          xs1  = zipWith (\ s x -> (s + int2float w1 * x) / int2float la) ss xs


stretch         :: Int -> [Double] -> [Double]
stretch ln xs
    = concat . snd . mapAccumL (wSum ln (length xs)) (0, 0.0) $ xs

resizeWidth     :: Int -> Picture -> Picture
resizeWidth w p
    | w == wa   = p
    | otherwise = map (stretch w) p
    where
    wa = widthMx p

resizeHeight    :: Int -> Picture -> Picture
resizeHeight h p
    | h == ha   = p
    | otherwise = concat
                  . snd
                  . mapAccumL
                        (wSums h (length p))
                        (0, (replicate (length (head p)) 0.0)) $ p
    where
    ha = heightMx p
