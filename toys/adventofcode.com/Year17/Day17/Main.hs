-- solution for
-- http://adventofcode.com/2017/day/17

module Main where

import Util.Main1    (main12)
import Data.Maybe
import Data.List (foldl', intercalate)
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Foldable

-- ----------------------------------------

main :: IO ()
main = main12 "2017-17"
       day17 process
       day17 process2  -- 2. part

-- ----------------------------------------

data Ring = R ! Int
              ! (Seq Int)
  deriving (Show)

singletonRing :: Int -> Ring
singletonRing = R 0 . S.singleton

addToRing :: Int -> Ring -> Ring
addToRing x (R ix s) =
  R ix' (S.insertAt ix' x s)
  where
    ix' = ix + 1

rotRing :: Int -> Ring -> Ring
rotRing n (R ix s) = R ((ix + n) `mod` S.length s) s

cur :: Ring -> Int
cur (R ix s) = S.index s ix

prettyRing :: Ring -> String
prettyRing (R ix s) =
  intercalate " " $
  (map show . toList . S.take  ix      $ s)
  ++
  ["(" ++ show (S.index s ix) ++ ")"]
  ++
  (map show . toList . S.drop (ix + 1) $ s)

spinningRing :: Int -> Int -> Ring
spinningRing steps n =
  foldl' (flip ins) (singletonRing 0) [1..n]
  where
    ins :: Int -> Ring -> Ring
    ins i
      | i == n    =                 addToRing i
      | otherwise = rotRing steps . addToRing i

nextRingElem :: Ring -> Int
nextRingElem = cur . rotRing 1

nextToN :: Int -> Ring -> Int
nextToN x (R _ix s) =
  S.index s ix1
  where
    ix  = fromJust . S.elemIndexL x $ s
    ix1 = (ix + 1) `mod` S.length s

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

spinlock, spinlock2 :: Int -> Int

spinlock  = nextRingElem . flip spinningRing 2017

spinlock2 = nextToN 0 . flip spinningRing (50 * 10^6)

-- ----------------------------------------

res, day17, t17, r17 :: String

res   = process day17
day17 = "366"

t17 = "3"
e17 = "638"
r17 = process t17

-- ----------------------------------------
