-- solution for
-- http://adventofcode.com/2017/day/15

module Main where

import Util.Main1    (main12)

import Data.Bits(Bits(..))

-- ----------------------------------------

main :: IO ()
main = main12 "2017-15"
       day15 randSeq
       day15 randSeq2

-- ----------------------------------------

randSeq :: String -> String
randSeq = show . process 40000000 . fromString

randSeq2 :: String -> String
randSeq2 = show . process2 5000000 . fromString

process :: Int -> (Int, Int, Int, Int) -> Int
process rep (factorA, factorB, seedA, seedB) =
  sum
  . take rep
  $ zipWith eq16 seq1 seq2
  where
    seq1 = tail $ iterate (rand factorA) seedA
    seq2 = tail $ iterate (rand factorB) seedB

process2 :: Int -> (Int, Int, Int, Int) -> Int
process2 rep (factorA, factorB, seedA, seedB) =
  sum
  . take rep
  $ zipWith eq16 seq1 seq2
  where
    seq1 = filter (multiple 4) . tail $ iterate (rand factorA) seedA
    seq2 = filter (multiple 8) . tail $ iterate (rand factorB) seedB

multiple :: Int -> Int -> Bool
multiple m x = x `mod` m == 0

eq16 :: Int -> Int -> Int
eq16 x y = fromEnum $ x .&. 0xFFFF == y .&. 0xFFFF

rand :: Int -> Int -> Int
rand factor prev =
  (prev * factor) `rem` 0x7fffffff

fromString :: String -> (Int, Int, Int, Int)
fromString s = (factorA, factorB, seedA, seedB)
  where
    (seedA : seedB : _) = map read . map last . map words . lines $ s
    factorA = 16807
    factorB = 48271

-- ----------------------------------------

exInp, exRes, exRes2, exRes', exRes2' :: String
exInp = "Generator A starts with 65\nGenerator B starts with 8921"
exRes = "588"
exRes2 = "309"

-- test case
exRes' = show . process 5 $ fromString exInp -- "1"
exRes2' = randSeq2 exInp

day15, res15 :: String
day15 = "Generator A starts with 512\nGenerator B starts with 191"
res15 = "567"


-- ----------------------------------------
