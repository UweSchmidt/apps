-- solution for
-- http://adventofcode.com/2017/day/10

module Main where

import Util.Main1    (main1)
import Data.Char     (isDigit, isSpace)
import Data.List     (foldl')

-- ----------------------------------------

main :: IO ()
main = main1 day10 process'

-- ----------------------------------------

data Ring a =
  Ring { _rot  :: Int
       , _len  :: Int
       , _vals :: [a]
       }
  deriving (Show)

mkRing :: [a] -> Ring a
mkRing xs = Ring 0 (length xs) xs

rotateRing :: Int -> Ring a -> Ring a
rotateRing n0 r@(Ring rot len vals)
  | n == 0    = r
  | otherwise = Ring ((rot + n) `mod` len) len (ys ++ xs)
  where
    n = n0 `mod` len
    (xs, ys) = splitAt n vals

normalRing :: Ring a -> Ring a
normalRing r@(Ring rot len v) =
  rotateRing (len - rot) r

reversePrefix :: Int -> Ring a -> Ring a
reversePrefix n0 r@(Ring rot len val)
  | n <= 1 = r
  | otherwise = Ring rot len $ reverse xs ++ ys
  where
    n = n0 `mod` len
    (xs, ys) = splitAt n val

-- ----------------------------------------

process' :: String -> String
process' = show . process 256 . fromString

process :: Int -> [Int] -> Int
process len =
    product
  . take 2
  . _vals
  . shuffle len

shuffle :: Int -> [Int] -> Ring Int
shuffle len =
    normalRing
  . foldl' tieKnot (mkRing [0 .. len-1])
  . zip [0..]
  where
    tieKnot :: Ring Int -> (Int, Int) -> Ring Int
    tieKnot r (step, width) =
      rotateRing (width + step) $ reversePrefix width r

-- ----------------------------------------

fromString :: String -> [Int]
fromString s = read $ "[" ++ s ++ "]"

ex :: Int
ex = process 5 [3, 4, 1, 5]

-- result of input day11
res :: String
res = process' day10

-- test input from adventofcode
day10 :: String
day10 = "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63"

-- ----------------------------------------
