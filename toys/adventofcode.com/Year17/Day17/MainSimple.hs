-- solution for
-- http://adventofcode.com/2017/day/17

module Main where

import Util.Main1    (main12)
import Data.List (foldl', intercalate)

-- ----------------------------------------

main :: IO ()
main = main12 "2017-17"
       day17 process
       day17 process2  -- 2. part

-- ----------------------------------------

data Ring a = R Int [a] [a]
  deriving (Show)

singletonRing :: a -> Ring a
singletonRing a = R 1 [] [a]

addToRing :: a -> Ring a -> Ring a
addToRing x (R l ls (c : rs)) =
  R (l + 1) (c : ls) (x : rs)

rotRing :: Int -> Ring a -> Ring a
rotRing n (R l ls rs) = R l ls' rs'
  where
    (ls', rs') = rot (n `mod` l) ls rs

    rot i ls []       = rot i [] (reverse ls)
    rot 0 ls rs       = (ls, rs)
    rot i ls (x : rs) = rot (i - 1) (x : ls) rs

cur :: Ring a -> a
cur (R _ _ (x : _)) = x

prettyRing :: Show a => Ring a -> String
prettyRing (R _l ls (x : rs)) =
  intercalate " " $
  (map show $ reverse ls)
  ++ ["(" ++ show x ++ ")"] ++
  (map show rs)

spinningRing :: Int -> Int -> Ring Int
spinningRing steps n = foldl' (flip ins) (singletonRing 0) [1..n]
  where
    ins :: Int -> Ring Int -> Ring Int
    ins i
      | i == n    =                 addToRing i
      | otherwise = rotRing steps . addToRing i

nextRingElem :: Ring Int -> Int
nextRingElem = cur . rotRing 1

-- ----------------------------------------

process :: String -> String
process = toString . spinlock . fromString

process2 :: String -> String
process2 = toString . spinlock2 . fromString

fromString :: String -> Int
fromString = read

toString :: Int -> String
toString = show

-- ----------------------------------------

spinlock :: Int -> Int
spinlock = nextRingElem . flip spinningRing 2017


spinlock2 = spinlock

-- ----------------------------------------

res, day17, t17, r17 :: String

res   = process day17
day17 = "366"

t17 = "3"
e17 = "638"
r17 = process t17

-- ----------------------------------------
