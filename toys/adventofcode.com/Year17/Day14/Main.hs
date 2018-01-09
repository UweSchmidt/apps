-- solution for
-- http://adventofcode.com/2017/day/14

module Main where

import Util.Main1    (main12)

-- TyingTheKnot is used in Year17.Day14
import Year17.Day10.TyingTheKnot(knotHash)
import Text.Printf (printf)
import Numeric (readHex)
import Data.Relation (Rel')
import qualified Data.Relation as R

-- ----------------------------------------

main :: IO ()
main = main12 "2017-14"
       day14 diskUsage
       day14 part2

-- ----------------------------------------

diskUsage :: String -> String
diskUsage = show . process . fromString

process :: String -> Int
process = sum
  . map (sum . map fromEnum)
  . binSquare

binSquare :: String -> [[Bool]]
binSquare key =
    map (map (== '1'))
  . map hexToBin
  . map knotHash
  . map (\i -> key ++ "-" ++ show i)
  $ [0..127::Int]

hexToBin :: String -> String
hexToBin hex = printf "%0128b" i
  where
    i :: Integer
    i = fst . head . readHex $ hex

fromString :: String -> String
fromString = id


part2 :: String -> String
part2 = show . connected . binSquare

type Connected = Rel' Int

connected :: [[Bool]] -> Int
connected sq =
  length
  . R.connectedComponents
  . R.trClosure
  $ conH `R.union` conV `R.union` conS
  where
    conH = connectedH rows cols sq
    conV = connectedV rows cols sq
    conS = singles    rows cols sq

    cols = length (head sq)
    rows = length sq

singles, connectedH, connectedV :: Int -> Int -> [[Bool]] -> Connected

-- connect all to itself
singles rows cols =
  R.unions . zipWith toCon [0.. rows - 1]
  where
    toCon :: Int -> [Bool] -> Connected
    toCon rcnt row = R.unions $ zipWith pair [0..] row
      where
        pair i x0
          | x0        = R.singleton ix ix
          | otherwise = R.empty
          where
            ix = rcnt * cols + i

-- connect all neighbours in a row
connectedH rows cols =
  R.unions . zipWith toCon [0.. rows - 1]
  where
    toCon :: Int -> [Bool] -> Connected
    toCon rcnt row = R.unions $ zipWith3 pair [0..] row (tail row)
      where
        pair i x0 x1
          | x0 && x1  = R.singleton ix (ix + 1)
          | otherwise = R.empty
          where
            ix = rcnt * cols + i

-- connect al neighbours in a column
connectedV rows cols sq =
  R.unions . map toCon $ zip3 [0.. rows - 1] sq (tail sq)
  where
    toCon :: (Int, [Bool], [Bool]) -> R.Rel Int Int
    toCon (rcnt, r0, r1) = R.unions $ zipWith3 pair [0.. cols - 1] r0 r1
      where
        pair j y0 y1
          | y0 && y1 = R.singleton ix (ix + cols)
          | otherwise = R.empty
          where
            ix = rcnt * cols + j

-- ----------------------------------------

exKey, exRes, exRes2 :: String
exKey  = "flqrgnkx"
exRes  = "8108"
exRes2 = "1242"

day14, res14 :: String
day14 = "hxtvlmkl"
res14 = "8214"

-- ----------------------------------------
