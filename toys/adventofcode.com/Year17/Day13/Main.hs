{-# LANGUAGE TupleSections #-}
-- solution for
-- http://adventofcode.com/2017/day/13

module Main where

import Util.Main1    (main12)
import Data.Char     (isDigit, isSpace)
import Data.List     (foldl')

-- ----------------------------------------


main :: IO ()
main = main12
       day13 firewall1
       day13 firewall2

-- ----------------------------------------

firewall1 :: String -> String
firewall1 = show . process . fromString

process :: [(Int, Int)] -> Int
process =
  foldl' severe 0 . toWall
  where
    severe acc ((i, d2), xs)
      | xs !! (i `mod` d2) == 0 = acc + i * d2
      | otherwise               = acc

type Firewall = [((Int, Int), [Int])]

toWall :: [(Int, Int)] -> [((Int, Int), [Int])]
toWall = map (\(i, d) -> ((i, 2 * (d-1)), toSeq d))

toSeq :: Int -> [Int]
toSeq n
  | n >= 2 = [0 .. n-1] ++ reverse [1 .. n-2]
  | otherwise = error "firewall: filter size < 2"

-- ----------------------------------------

fromString :: String -> [(Int, Int)]
fromString =
    map (\(x1 : x2 : _) -> (x1, x2))
  . map (map read . words)
  . map (filter (\c -> isDigit c || isSpace c ))
  . lines

-- ----------------------------------------

firewall2 :: String -> String
firewall2 = show . process2 (10^7) . fromString

-- without mx this might not terminate
-- now it can terminate with head of empty list
-- which is slightly better

process2 :: Int -> [(Int, Int)] -> Int
process2 mx cf =
    head
  . map fst
  . filter (uncurry isNotCaught)
  . map (,fw) $ [0..mx]
  where
    fw = toWall cf

isNotCaught :: Int -> Firewall -> Bool
isNotCaught delay =
  not . foldr caught False
  where
    caught :: ((Int, Int), [Int]) -> Bool -> Bool
    caught ((i, d2), xs) acc =
      xs !! i' == 0 || acc
      where
        i' = (i + delay) `mod` d2

-- ----------------------------------------
--
-- result of input day11
exp1, res1 :: String
exp1 = "3360"
res1 = firewall1 day13

ex :: String
ex = "0: 3\n1: 2\n4: 4\n6: 4"

-- test input from adventofcode
day13 :: String
day13 = "0: 5\n1: 2\n2: 3\n4: 4\n6: 6\n8: 4\n10: 8\n12: 6\n14: 6\n16: 8\n18: 6\n20: 9\n22: 8\n24: 10\n26: 8\n28: 8\n30: 12\n32: 8\n34: 12\n36: 10\n38: 12\n40: 12\n42: 12\n44: 12\n46: 12\n48: 14\n50: 12\n52: 14\n54: 12\n56: 14\n58: 12\n60: 14\n62: 14\n64: 14\n66: 14\n68: 14\n70: 14\n72: 14\n76: 14\n80: 18\n84: 14\n90: 18\n92: 17\n"

exp2, res2 :: String
exp2 = "3850260"
res2 = firewall2 day13

-- ----------------------------------------
