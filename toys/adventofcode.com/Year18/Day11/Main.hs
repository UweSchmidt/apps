{-# LANGUAGE TupleSections #-}
-- solution for
-- http://adventofcode.com/2018/day/9

module Main where

import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.List
import           Util.Main1 (main12)
-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-11"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = toString . solve1 . fromString

captcha2 :: String -> String
captcha2 = toString2 . solve2 . fromString

-- ----------------------------------------

type Cell      = (Int, Int)
type Power     = Int
type FuelCells = Map Cell Power

gridSize :: Int
gridSize = 300

cellCoords' :: Int -> [Cell]
cellCoords' size = [(x, y) | x <- [1..size], y <- [1..size]]

cellCoords :: [Cell]
cellCoords = cellCoords' gridSize

cellCoords'3 :: [Cell]
cellCoords'3 = cellCoords' (gridSize - 2)

powers :: Int -> [Cell] -> FuelCells
powers serialNo cls =
  foldl' ins M.empty cls
  where
    ins m c = M.insert c (powerCell serialNo c) m

powerCell :: Int -> Cell -> Power
powerCell serialNo (x, y) =
  (rackID * y + serialNo) * rackID `div` 100 `mod` 10 - 5
  where
    rackID = x + 10

powerSquare'n :: Int -> FuelCells -> Cell -> Power
powerSquare'n n cls (x, y) =
  sum . map (cls M.!) $ [(x', y')
                      | x' <- [x .. x + (n - 1)]
                      , y' <- [y .. y + (n - 1)]
                      ]

powerSquare'ns :: Int -> FuelCells -> ((Cell, Int), Power)
powerSquare'ns n cls =
  (\(x1, x2) -> ((x1,n), x2))
  .
  foldl' maxp ((0,0), minBound)
  .
  map toP
  $
  cellCoords' (gridSize - n + 1)
  where
    toP c = (c, powerSquare'n n cls c)

maxp :: (a, Int) -> (a, Int) -> (a, Int)
maxp (c0, p0) (c1, p1)
  | p0 >= p1  = (c0, p0)
  | otherwise = (c1, p1)

powerSquare3s :: FuelCells -> (Cell, Power)
powerSquare3s = (\((x1, _x2), x3) -> (x1, x3)) . powerSquare'ns 3

powerSquareAlls :: FuelCells -> ((Cell, Int), Power)
powerSquareAlls cls =
  foldl1' maxp . map (flip powerSquare'ns cls) $ [1 .. gridSize]

solve1 :: Int -> Cell
solve1 serialNo = fst . powerSquare3s . powers serialNo $ cellCoords

solve2' :: Int -> (Cell, Int)
solve2' serialNo = fst . powerSquareAlls . powers serialNo $ cellCoords

{- brute force solution for solve2' :: Int -> ((Cell, Int), Power)

uwe@mecki:~/haskell/apps/toys/adventofcode.com> time 2018-11 -2
236,175,11

real    160m53.121s      -- over 2 hours an 40 minutes
user    162m3.063s
sys     1m17.762s
-}

-- ----------------------------------------

{- this iterative approach is a bit more efficient

uwe@mecki:~/haskell/apps/toys/adventofcode.com> time 2018-11 -2
236,175,11

real    0m14.920s         -- about 15 seconds
user    0m21.101s
sys     0m2.274s
-}


solve2 :: Int -> (Cell, Int)
solve2 serialNo = fst . foldl1' maxp . take 300 . drop 1 $ pmMax
  where
    pm0, pm1 :: FuelCells
    pm0 = M.empty                     -- 0x0 square power map
    pm1 = powers serialNo cellCoords  -- 1x1 square power map

    pms :: [(Int, FuelCells)]
    pms = (0, pm0) : (1, pm1) : zipWith (powerSquare pm1) (tail pms) pms

    pmMax :: [((Cell, Int), Power)]
    pmMax = map toMax pms

    toMax :: (Int, FuelCells) -> ((Cell, Int), Power)
    toMax (sz, pm) = ((c, sz), p)
      where
        (c, p) = M.foldlWithKey' maxp' ((0,0), minBound) pm
          where
            maxp' mx c' p' = mx `maxp` (c', p')

powerSquare :: FuelCells -> (Int, FuelCells) -> (Int, FuelCells) -> (Int, FuelCells)
powerSquare m1 (i1, pm1) (i2, pm2) = (i0, pm0)
  where
    i0      = i1 + 1

    at :: FuelCells -> Cell -> Power
    at m k = fromMaybe 0 $ M.lookup k m

    pm0 :: FuelCells
    pm0 = foldl' insPower M.empty $ cellCoords' (gridSize - i0 + 1)

    insPower :: FuelCells -> Cell -> FuelCells
    insPower m c0@(x, y) = M.insert c0 pow m
      where
        c1  = (x + 1, y + 1)
        pow = pm1 `at` c0
              +
              pm1 `at` c1
              -
              pm2 `at` c1
              +
              m1 `at` (x, y + i1)   -- left bottom corner
              +
              m1 `at` (x + i1, y)   -- right top corner

-- ----------------------------------------

toString :: Cell -> String
toString (x, y) = show x ++ "," ++ show y

toString2 :: (Cell, Int) -> String
toString2 ((x, y), s) = show x ++ "," ++ show y ++ "," ++ show s

fromString :: String -> Int
fromString = read

ex :: String
ex = "8"

inp :: String
inp = "3628"
