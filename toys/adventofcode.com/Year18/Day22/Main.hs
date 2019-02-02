{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns  #-}

-- solution for
-- http://adventofcode.com/2018/day/22

module Main where

import Control.Lens
import Data.Array.IArray
import Data.Foldable

import Linear.V2

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Util.Main1      (main12)

import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-22"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

solve1 :: Input -> Int
solve1 inp@(depth, _target) =
  sum . map toType . toList . mkGeoIndices $ inp
  where
    toType :: Int -> Int
    toType i = mod20183 (i + depth) `mod` 3

toRegionType :: Int -> RegionType
toRegionType = toEnum . (`mod` 3)

timesX, timesY :: Int
timesX = 16807
timesY = 48271

mod20183 :: Int -> Int
mod20183 = (`mod` 20183)


mkGeoIndices :: Input -> GeoIndices
mkGeoIndices (depth, target@(V2 tx ty)) = geoIndices
  where
    timesX = 16807
    timesY = 48271

    mod20183 = (`mod` 20183)

    erosionLev :: Point -> Int
    erosionLev p =
      mod20183 $ geoIndices ! p + depth

    geoIndices :: GeoIndices
    geoIndices =
      array (pure 0, target) $
      [ (pure 0, 0)
      , (target, 0)
      ]
      ++
      [ (x `V2` 0, x * timesX) | x <- [1 .. tx]]
      ++
      [ (0 `V2` y, y * timesY) | y <- [1 .. ty]]
      ++
      [ (x `V2` y, erosionLev (pred x `V2` y)
                   *
                   erosionLev (x `V2` pred y)
        )
      | x <- [1 .. tx]
      , y <- [1 .. ty]
      , not (x == tx && y == ty)
      ]


solve2 :: Input -> Int
solve2 = undefined

-- ----------------------------------------

type Point = V2 Int

type GeoIndices = Array (Point) Int

data RegionType = Rocky | Wet | Narrow
                deriving (Enum, Eq, Ord, Show)

type Input = (Int, Point)

type SP = Parsec () String

fromString :: String -> Input
fromString s = maybe (error "no parse") id $ parseMaybe pInput s

pInput :: SP Input
pInput = (,) <$> parse1 <*> parse2
  where
    parse1 :: SP Int
    parse1 = string "depth: " *> num <* nl

    parse2 :: SP Point
    parse2 = V2 <$> (string "target: " *> num)
                <*> (char ',' *> num <* optional nl)

    num :: SP Int
    num = read <$> some digitChar

    nl = char '\n'

-- ----------------------------------------

ex1 :: String
ex1 = "depth: 510\ntarget: 10,10\n"

inp' :: IO String
inp' = readFile "Year18/Day22/day22.txt"

inp :: String
inp = "depth: 7863\ntarget: 14,760\n"

-- ----------------------------------------
