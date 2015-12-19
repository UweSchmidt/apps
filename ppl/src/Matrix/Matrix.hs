-- $Id: Matrix.hs,v 1.12 2006/12/23 13:32:08 uwe Exp $

module Matrix.Matrix
    ( Matrix

    , nullMx
    , zeroMx

    , dimMx
    , widthMx
    , heightMx

    , flipHMx
    , flipVMx
    , flipDMx
    , rotateMx
    , shiftMx

    , cutMx
    , cropMx
    , pasteMx
    , scaleMx
    , replicateMx
    , sideBySideMx
    , aboveMx

    , partHMx
    , partVMx
    , splitHMx
    , splitVMx
    , mergeHMx
    , mergeVMx
    , concatHMx
    , concatVMx

    , mapMx
    , mapPairMx
    , zipWithMx
    ) where

import Data.List

type Matrix a           = [[a]]

type TransformMatrix a  = Matrix a -> Matrix a

type CombineMatrix a    = Matrix a -> Matrix a -> Matrix a

-- -------------------------------------------------------------------
-- zeroMx

-- empty matrix

nullMx          :: Matrix a
nullMx          = []

zeroMx          :: TransformMatrix a
zeroMx _        = nullMx

-- -------------------------------------------------------------------

-- apply a function to all elements of a matrix

mapMx           :: (a -> b) -> Matrix a -> Matrix b
mapMx f = map (map f)

-- -------------------------------------------------------------------

-- compute a matrix by elementwise application of a binar function to 2 maticies
-- the matricies should have equal dimensions

zipWithMx       :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWithMx f     = zipWith (zipWith f)


-- -------------------------------------------------------------------

-- convert a matrix of pairs into a matrix of values

mapPairMx       :: (a -> b -> c) -> Matrix (a,b) -> Matrix c
mapPairMx f
    = mapMx (uncurry f)

-- -------------------------------------------------------------------

-- dimensions

dimMx           :: Matrix a -> (Int, Int)

dimMx []        = (0, 0)
dimMx ([]:_)    = (0, 0)
dimMx m@(r1:_)  = (length r1, length m)

widthMx         :: Matrix a -> Int
heightMx        :: Matrix a -> Int

widthMx         = fst . dimMx
heightMx        = snd . dimMx

-- -------------------------------------------------------------------

-- flip the picture horizontally, vertically, and transpose it

flipHMx         :: TransformMatrix a
flipHMx         = reverse

flipVMx         :: TransformMatrix a
flipVMx         = map reverse

flipDMx         :: TransformMatrix a
flipDMx         = transpose

rotateMx        :: TransformMatrix a
rotateMx        = flipHMx . flipVMx

scaleMx         :: Int -> Int -> TransformMatrix a
scaleMx w h
    | w <= 0 || h <= 0
        = zeroMx
    | otherwise
        = scaleV h . scaleH w

replicateMx     :: Int -> Int -> TransformMatrix a
replicateMx w h
    | w <= 0 || h <= 0
        = zeroMx
    | otherwise
        = replicateV h . replicateH w

sideBySideMx    :: CombineMatrix a
sideBySideMx    = zipWith (++)

aboveMx         :: CombineMatrix a
aboveMx         = (++)

cutMx           :: Int -> Int -> Int -> Int -> TransformMatrix a
cutMx x y w h
    | x < 0
        = cutMx 0 y ((w + x) `max` 0) h
    | y < 0
        = cutMx x 0 w ((h + y) `max` 0)
    | w <= 0 || h <= 0
        = zeroMx
    | otherwise
        = map (take w . drop x) . take h . drop y

cropMx          :: Int -> Int -> Int -> Int -> TransformMatrix a
cropMx x y w h
    | x < 0
        = cropMx 0 y w h . map (drop (0 - x))
    | y < 0
        = cropMx x 0 w h . drop (0 - y)
    | otherwise
        = cutMx 0 0 (w - x) (h - y)

pasteMx         :: Int -> Int -> CombineMatrix a
pasteMx x y p1 p2
    = top ++ mid' ++ bot
    where
    (w2, h2)    = dimMx p2
    p1'         = cropMx x y w2 h2 p1
    (w1, h1)    = dimMx p1'
    (top, rest) = splitAt y  p2
    (mid, bot)  = splitAt h1 rest
    mid'        = zipWith pasteRow p1' mid
    pasteRow r1 r2
        = left ++ r1 ++ right
        where
        (left, rest1)   = splitAt x r2
        right           = drop w1 rest1

shiftMx         :: Int -> Int -> TransformMatrix a
shiftMx n m p
    | w == 0 || h == 0
        = p
    | otherwise
        = shiftV (m `mod` h) . shiftH (n `mod` w) $ p
    where
    (w, h) = dimMx p

partHMx         :: Int -> Matrix a -> [Matrix a]
partHMx n
    = part n

partVMx         :: Int -> Matrix a -> [Matrix a]
partVMx n
    = transpose . map (part n)

splitHMx                :: Int -> Matrix a -> [Matrix a]
splitHMx n m
    = split (heightMx m `div` n) m

splitVMx                :: Int -> Matrix a -> [Matrix a]
splitVMx n m
    = transpose . map (split (widthMx m `div` n)) $ m

mergeHMx        :: [Matrix a] -> Matrix a
mergeHMx
    = concat . transpose

mergeVMx        :: [Matrix a] -> Matrix a
mergeVMx
    = merge
      where
      merge s
          | null . head $ s
              = []
          | otherwise
              = (concat . transpose . map head $ s) : (merge . map tail $ s)

concatHMx       :: [Matrix a] -> Matrix a
concatHMx
    = concat

concatVMx       :: [Matrix a] -> Matrix a
concatVMx
    = conc
      where
      conc s
          | null . head $ s
              = []
          | otherwise
              = (concat . map head $ s) : (conc . map tail $ s)

-- -------------------------------------------------------------------

-- local functions

-- -------------------------------------------------------------------

-- transpose the picture

{-
transpose       :: TransformMatrix a
transpose ([]:_)
    = []
transpose m
    = let
      row       = foldr (:) [] (map head m)
      rest      = map tail m
      in
      row : transpose rest
-}
-- -------------------------------------------------------------------

scaleH          :: Int -> TransformMatrix a
scaleH 1        = id
scaleH n        = map (concatMap (replicate n))

scaleV          :: Int -> TransformMatrix a
scaleV 1        = id
scaleV n        = concatMap (replicate n)

replicateH      :: Int -> TransformMatrix a
replicateH 1    = id
replicateH n    = map (concat . replicate n)

replicateV      :: Int -> TransformMatrix a
replicateV 1    = id
replicateV n    = concat . replicate n

shiftV          :: Int -> [a] -> [a]
shiftV 0 p      = p
shiftV n p      = drop n p ++ take n p

shiftH          :: Int -> [[a]] -> [[a]]
shiftH 0        = id
shiftH n        = map (shiftV n)

-- -------------------------------------------------------------------

part            :: Int -> [a] -> [[a]]
part n []
    = replicate n []

part n s
    = zipWith (:) l (part n r)
      where (l, r) = splitAt n s


split           :: Int -> [a] -> [[a]]
split _ []
    = []

split n s
    = l : split n r
      where (l, r) = splitAt n s

-- -------------------------------------------------------------------
