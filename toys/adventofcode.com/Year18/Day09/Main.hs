-- solution for
-- http://adventofcode.com/2018/day/9

module Main where

import           Data.Char
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List
import           Util.Main1 (main12)
import           Control.Arrow (second)
-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-08"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

type Circle = ([Int], [Int])
type Scores = IntMap Int

circle0 :: Circle
circle0 = ([], [0])

advance :: Circle -> Circle
advance (ls, c : rs) = swap (c : ls, rs)
  where
    swap (ls, []) = ([], reverse ls)
    swap circ     = circ

back :: Circle -> Circle
back (c : ls, rs) = (ls, c : rs)
back ([],     rs) = (rs' , [c])
  where
    (c : rs') = reverse rs

forward :: Int -> Circle -> Circle
forward n = nTimes n advance

backward :: Int -> Circle -> Circle
backward n = nTimes n back

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes n f = foldr (.) id $ replicate n f

dropCur :: Circle -> (Int, Circle)
dropCur (ls, c : rs) = (c, (ls, rs))

addCur :: Int -> Circle -> Circle
addCur i (ls, rs) = (ls, i : rs)

insMarble :: Int -> Circle -> Circle
insMarble c = addCur c . forward 2

insMarbles :: [Int] -> Circle -> Circle
insMarbles is = foldl (\ f c -> insMarble c . f) id is

step :: (Int, Int) -> (Circle, Scores) -> (Circle, Scores)
step (player, i) (cs, scm)
  | i `mod` 23 /= 0 = (insMarble i cs, scm)
  | otherwise       = (cs', scm')
  where
    (j, cs') = dropCur . backward 7 $ cs
    scm'     = M.insertWith (+) player (i + j) scm

steps :: [(Int, Int)] -> (Circle, Scores) -> (Circle, Scores)
steps = flip $ foldl' (flip step)

solve1 :: (Int, Int) -> Int
solve1 (players, maxMarble) =
  maximum . map snd . M.toList $ sc1
  where
    (_cs1, sc1) = steps ms0 (circle0, sc0)
    ms0         = take maxMarble $ zip (cycle [1 .. players]) [1..]
    sc0         = M.empty

solve2 :: (Int, Int) -> Int
solve2 = solve1 . second (100 *)

-- ----------------------------------------

fromString :: String -> (Int, Int)
fromString =
  toTuple3 . map read . filter (all isDigit) . words
  where
    toTuple3 (x1 : x2 : _xs2) = (x1, x2)

-- ----------------------------------------

exs :: [String]
ex1, ex2, ex3, ex4, ex5 :: String
exs@[ex1,ex2,ex3,ex4,ex5] =
  [ "10 players; last marble is worth 1618 points: high score is 8317"
  , "13 players; last marble is worth 7999 points: high score is 146373"
  , "17 players; last marble is worth 1104 points: high score is 2764"
  , "21 players; last marble is worth 6111 points: high score is 54718"
  , "30 players; last marble is worth 5807 points: high score is 37305"
  ]

inp :: String
inp = "464 players; last marble is worth 71730 points"
