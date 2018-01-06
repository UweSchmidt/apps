-- solution for
-- http://adventofcode.com/2017/day/13

module Main where

import Util.Main1    (main1)
import Data.Char     (isDigit, isSpace)
import Data.List     (foldl')
import Control.Arrow (second)

-- ----------------------------------------


main :: IO ()
main = main1 day13 process'

-- ----------------------------------------

process' :: String -> String
process' = show . process . fromString

process :: [(Int, Int)] -> Int
process =
  foldl' severe 0 . map (\w -> (w , toSeq $ snd w))
  where
    severe acc ((i, d), xs)
      | xs !! i == 0 = acc + i * d
      | otherwise    = acc

toSeq :: Int -> [Int]
toSeq n = cycle xs
      where
        xs = [0 .. n-1] ++ reverse [1 .. n-2]

-- ----------------------------------------

fromString :: String -> [(Int, Int)]
fromString = id
  . map (\(x1:x2:_) -> (x1, x2))
  . map (map read . words)
  . map (filter (\c -> isDigit c || isSpace c ))
  . lines


-- result of input day11
res :: String
res = process' day13

ex :: String
ex = "0: 3\n1: 2\n4: 4\n6: 4"

-- test input from adventofcode
day13 :: String
day13 = "0: 5\n1: 2\n2: 3\n4: 4\n6: 6\n8: 4\n10: 8\n12: 6\n14: 6\n16: 8\n18: 6\n20: 9\n22: 8\n24: 10\n26: 8\n28: 8\n30: 12\n32: 8\n34: 12\n36: 10\n38: 12\n40: 12\n42: 12\n44: 12\n46: 12\n48: 14\n50: 12\n52: 14\n54: 12\n56: 14\n58: 12\n60: 14\n62: 14\n64: 14\n66: 14\n68: 14\n70: 14\n72: 14\n76: 14\n80: 18\n84: 14\n90: 18\n92: 17\n"

-- ----------------------------------------
