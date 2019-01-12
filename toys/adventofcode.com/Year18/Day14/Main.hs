{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/14

module Main where

import           Data.Sequence (Seq)
import qualified Data.Sequence as S
import           Data.List
import           Util.Main1 (main12)

-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-14"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = toString . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

type RecList  = Seq Int
type Recipes = ((Int, Int), RecList)

rec0 :: Recipes
rec0 = ((0, 1), S.fromList [3,7])

step :: Recipes -> Recipes
step ((i, j), rs) =
  ((i', j'), rs')
  where
    l  = S.length rs'

    x     = rs `S.index` i
    y     = rs `S.index` j

    i' = (i + 1 + x) `mod` l
    j' = (j + 1 + y) `mod` l

    rs'   = foldl' (S.|>) rs . map toI $ show (x + y)

steps :: Int -> Recipes -> Recipes
steps 0 = id
steps n = steps (n-1) . step

-- ----------------------------------------
-- 2. part
-- rewrite stepn such that the final sequence is generated lazily
-- idices i and j are only needed for extending the sequence

step' :: Int -> Int -> RecList -> [Int]
step' i j rs =
  new ++ step' i' j' rs'
  where
    l   = S.length rs'

    x   = rs `S.index` i
    y   = rs `S.index` j

    i'  = (i + 1 + x) `mod` l
    j'  = (j + 1 + y) `mod` l

    new = map toI $ show (x + y)

    rs' = foldl' (S.|>) rs $ new

toI :: Char -> Int
toI c = fromEnum c - fromEnum '0'

-- ----------------------------------------

solve1 :: Int -> [Int]
solve1 n =  foldr (:) [] . S.take 10 . S.drop n . snd . steps (n + 10 - 2) $ rec0

solve2 :: Int -> Int
solve2 n =
  fst . head . filter ((n' `isPrefixOf`) . snd) $ zip [0..] (tails recipes)
  where
    recipes      = 3 : 7 : step' i j rs
    ((i, j), rs) = rec0
    n' = map toI . show $ n

-- ----------------------------------------

toString :: [Int] -> String
toString = concat . map show

fromString :: String -> Int
fromString = read

ex :: String
ex = "9"

inp' :: IO String
inp' = readFile "Year18/Day13/day13.txt"

inp :: String
inp = "505961"
