-- solution for
-- http://adventofcode.com/2018/day/5


module Main where

import           Data.Char
import           Util.Main1 (main12)

-- ----------------------------------------

main :: IO ()
main = do
  inp <- inp'
  main12 "2018-05"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

solve1 :: String -> Int
solve1 = length . go []
  where
    go ls []
      = reverse ls
    go (l : ls) (r : rs)
      | isLetter r
        &&
        abs (fromEnum r - fromEnum l) == 32
          = go ls rs
    go ls (r : rs)
      = go (r : ls) rs

solve2 :: String -> Int
solve2 xs = minimum . map (\ c -> solve1 . filter (isNotC c) $ xs) $ ['A' .. 'Z']

isNotC :: Char -> Char -> Bool
isNotC c x = x /= c && x /= toLower c

fromString :: String -> String
fromString = filter isLetter

-- ----------------------------------------

ex :: String
ex = "dabAcCaCBAcCcaDA"

inp' :: IO String
inp' = readFile "Year18/Day05/day05.txt"
