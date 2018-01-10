-- solution for
-- http://adventofcode.com/2017/day/15

module Main where

import Util.Main1    (main12)

-- TyingTheKnot is used in Year17.Day14
import Year17.Day10.TyingTheKnot(knotHash)
import Text.Printf (printf)
import Numeric (readHex)
import Data.Relation (Rel')
import qualified Data.Relation as R
import Data.Word(Word32)
import Data.Int(Int32)
import Data.Bits(Bits(..))
import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = main12 "2017-15"
       day15 randSeq
       day15 randSeq

-- ----------------------------------------

type IntXX = Int

randSeq :: String -> String
randSeq = show . process . fromString

process :: (IntXX, IntXX, IntXX, IntXX) -> Int
process (factor1, factor2, seed1, seed2) =
  sum
  . take 40
  . map (\p@(x, y) -> tr p $ fromEnum $ x .&. 0xFFFF == y .&. 0xFFFF)
  $ zip seq1 seq2
  where
    seq1 = tail $ iterate (rand factor1) seed1
    seq2 = tail $ iterate (rand factor2) seed2

tr p@(x,y) = trace pp . trace (show p)
  where
    pp = "(" ++ printf "%032b" x ++ "," ++ printf "%032b" y ++ ")"

rand :: IntXX -> IntXX -> IntXX
rand factor prev = (prev * factor) `rem` 0x7fffffff

fromString :: String -> (IntXX, IntXX, IntXX, IntXX)
fromString s = read $ "(" ++ s ++ ")"

-- ----------------------------------------

exKey, exRes :: String
exKey  = "16807,48271,65,8921"
exRes  = "8108"


day15, res15 :: String
day15 = ""
res15 = ""

-- ----------------------------------------
