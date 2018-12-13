{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/12

module Main where

import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.List
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Util.Main1 (main12)
import           Control.Arrow ((***))
-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-12"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

type Pots       = [Bool]
type TruthTable = Vector Bool

solve1 :: (TruthTable, Pots) -> Int
solve1 (tt, ps) = sum $ solve' 20 tt ps

solve' :: Int -> TruthTable -> Pots -> [Int]
solve' gens tt ps = growingPots ps
  where
    growingPots =
      map fst . filter snd . zip [negate (2 * gens) .. ] . gen'n gens tt

-- the #'es can move at most 2 steps to the left and/or to the right
-- so the output row of pots has grown 2 steps to the left and 2 steps to the right

gen'n :: Int -> TruthTable -> Pots -> Pots
gen'n n tt ps = head . drop n $ iterate' (gen'1 tt) ps

gen'1 :: TruthTable -> Pots -> Pots
gen'1 tt ps = zipWith5 toB ps_2 ps_1 ps0 ps'1 ps'2
  where
    ps0  = False : False : ps ++ replicate 4 False
    ps_1 = False : ps0
    ps_2 = False : ps_1
    ps'1 = tail    ps0
    ps'2 = tail    ps'1

    toB :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
    toB b_2 b_1 b0 b'1 b'2 = tt V.! ix
      where
        int = fromEnum
        ix  = (((int b_2 * 2 + int b_1) * 2 + int b0) * 2 + int b'1) * 2 + int b'2

-- tricky, the generation runs at step 184 in a kind of cycle,
-- all #-positions are from then on just shifted by 1 step to the right
--
-- so the final positions can easily be computed by adding the diff
-- 50 * 10^9 - 184

solve2 :: (TruthTable, Pots) -> Int
solve2 (tt, ps) = sum . map (+ diff) $ pos
  where
    n :: Int
    n = 50 * 10^9  -- 50 billions

    diff = n - periodStart

    (periodStart, pos, _) =
      head . dropWhile (not . shifted) $ zip3 [1..] (tail poss) poss

    shifted (_n, xs1, xs0) =
      map (+ 1) xs0 == xs1

    poss =
      zipWith toPos [0..] $ iterate' (gen'1 tt) ps
      where
        toPos n' pots =
          map fst .filter snd . zip [negate (2 * n') .. ] $ pots

-- ----------------------------------------

toString :: Pots -> String
toString = map (\ b -> if b then '#' else '.')

fromString :: String -> (TruthTable, Pots)
fromString =
  swap . (parseInp *** parseTT ) . (\ (l1 : _ : ls) -> (l1, ls)). lines
  where
    parseInp =
      map (== '#') . drop (length "initial state: ")

    parseTT =
      V.fromList . map ((== '#') . head. drop (length "..... => ")) . reverse .  sort


ex :: String
ex = "initial state: #..#.#..##......###...###\n\n..... => .\n....# => .\n...#. => .\n...## => #\n..#.. => #\n..#.# => .\n..##. => .\n..### => .\n.#... => #\n.#..# => .\n.#.#. => #\n.#.## => #\n.##.. => #\n.##.# => .\n.###. => .\n.#### => #\n#.... => .\n#...# => .\n#..#. => .\n#..## => .\n#.#.. => .\n#.#.# => #\n#.##. => .\n#.### => #\n##... => .\n##..# => .\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #\n##### => .\n"

{-
..... => .
....# => .
...#. => .
...## => #
..#.. => #
..#.# => .
..##. => .
..### => .
.#... => #
.#..# => .
.#.#. => #
.#.## => #
.##.. => #
.##.# => .
.###. => .
.#### => #
#.... => .
#...# => .
#..#. => .
#..## => .
#.#.. => .
#.#.# => #
#.##. => .
#.### => #
##... => .
##..# => .
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
##### => .
-}

inp' :: IO String
inp' = readFile "Year18/Day12/day12.txt"

inp :: String
inp = "initial state: ##.##.#.#...#......#..#.###..##...##.#####..#..###.########.##.....#...#...##....##.#...#.###...#.##\n\n.###. => #\n###.# => #\n#..#. => #\n.#..# => #\n...## => #\n.#### => .\n.#.## => #\n#.... => .\n#..## => .\n..#.. => .\n#.##. => #\n##.#. => .\n....# => .\n#.#.. => #\n.#... => #\n.##.# => #\n..### => .\n.##.. => .\n##... => #\n###.. => #\n##..# => #\n...#. => .\n..#.# => #\n..##. => .\n#...# => .\n.#.#. => #\n##### => .\n#.#.# => .\n####. => #\n#.### => .\n..... => .\n##.## => .\n"
