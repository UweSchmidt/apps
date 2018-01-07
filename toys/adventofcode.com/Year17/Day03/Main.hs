-- solution for
-- http://adventofcode.com/2017/day/3

module Main where

import Util.Main1 (main1)
import Data.List  (foldl')

main :: IO ()
main = main1 day03 process'

process' :: String -> String
process' = show . process . fromString

-- ----------------------------------------

data Pos = P !Int !Int
  deriving (Eq, Ord, Show)

l,r,u,d :: Pos
r = P 1    0
l = P (-1) 0
u = P 0    1
d = P 0 (-1)

org :: Pos
org = P 0 0

add :: Pos -> Pos -> Pos
add (P x1 y1) (P x2 y2) = P (x1+x2) (y1+y2)

addS :: [Pos] -> Pos
addS = foldl' add org

sub :: Pos -> Pos -> Pos
sub (P x1 y1) (P x2 y2) = P (x1-x2) (y1-y2)

spiral :: [Pos]
spiral = org : (concat $ zipWith replicate seq2 seq1)
  where
    seq1 :: [Pos]
    seq1 = cycle [r, u, l, d]

    seq2 :: [Int]
    seq2 = foldr (\x xs -> x : x : xs) [] [1..]

manhattan :: Pos -> Int
manhattan (P x y) = abs x + abs y

process :: Int -> Int
process n =
  manhattan . addS . take n $ spiral

fromString :: String -> Int
fromString = read

res :: String
res = process' day03

day03 :: String
day03 = "361527"
