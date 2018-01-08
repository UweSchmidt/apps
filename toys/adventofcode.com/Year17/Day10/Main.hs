-- solution for
-- http://adventofcode.com/2017/day/10

module Main where

import Util.Main1    (main12)

-- TyingTheKnot is used in Year17.Day14
import Year17.Day10.TyingTheKnot

-- ----------------------------------------

main :: IO ()
main = main12 "2017-10"
       day10 tieKnots
       day10 knotHash  -- 2. part

-- ----------------------------------------
