{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/18

module Main where

import           Data.Array.IArray
import           Data.Array.Unboxed (UArray)
import           Data.List  (foldl', isPrefixOf)
import qualified Data.List.Period  as L
import           Util.Main1 (main12)
import           Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  -- inp <- inp'
  main12 "2018-18"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 10 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 600 1000000000 . fromString

-- ----------------------------------------

withTrace :: Bool
withTrace = True -- False

trace' :: String -> a -> a
trace' s
  | withTrace = trace s
  | otherwise = id

-- ----------------------------------------

type Acre  = Char
type Point = (Int, Int)
type Rect  = (Point, Point)

type Coll  = UArray Point Acre

type CAcc  = Array Point CFct
type CFct  = Coll -> Point -> [Acre]

openGround, trees, lumberYard :: Acre
openGround = '.'
trees      = '|'
lumberYard = '#'

nextGen :: CAcc -> Coll -> Coll
nextGen carr coll =
  -- trace (show $ cntWood coll') $
  coll'
  where
    coll'  = listArray bds
             [ next (x, y) | x <- [x0..x1], y <- [y0..y1]]
    next p = -- trace' (show p) $
             nextAcre (coll ! p) ((carr ! p) coll p)

    bds@((x0, y0), (x1, y1)) = bounds coll

nextAcre :: Acre -> [Acre] -> Acre
nextAcre c nbs
  | c == openGround
    &&
    threeOrMoreTrees = trees
  | c == trees
    &&
    threeOrMoreLumberYard = lumberYard
  | c == lumberYard
    &&
    not atLeat1Lumber1Tree = openGround
  | otherwise = c
  where
    threeOrMoreTrees =
      (>= 3) . length . filter (== trees) $ nbs

    threeOrMoreLumberYard =
      (>= 3) . length . filter (== lumberYard) $ nbs

    atLeat1Lumber1Tree =
      lumberYard `elem` nbs
      &&
      trees `elem` nbs

cArr :: Rect -> CAcc
cArr bds = array bds $
  [((  0, 0 ), ixNW)] ++                    -- corners
  [((_49, 0 ), ixNE)] ++
  [((  0,_49), ixSW)] ++
  [((_49,_49), ixSE)] ++

  [((  x, 0 ), ixN ) | x <- [1 .. _48]] ++   -- borders
  [((  0, y ), ixW)  | y <- [1 .. _48]] ++
  [((_49, y ), ixE)  | y <- [1 .. _48]] ++
  [((  x,_49), ixS ) | x <- [1 .. _48]] ++

  [(( x, y), ixXY) | x <- [1 .. _48]         -- inner cells
                   , y <- [1 .. _48]]

  where
    _49 = fst . snd $ bds
    _48 = _49 - 1

    ixa a is = -- trace' (show is) $
               map (a !) is

    ixXY a (x, y)
      = ixa a [ (x', y') | x' <- [x - 1 .. x + 1]
                         , y' <- [y - 1 .. y + 1]
                         , not (x' == x && y' == y)
                         ]

    ixW a (x, y)
      = ixa a [ (x', y') | x' <- [x     .. x + 1]
                         , y' <- [y - 1 .. y + 1]
                         , not (x' == x && y' == y)
                         ]

    ixN a (x, y)
      = ixa a [ (x', y') | x' <- [x - 1 .. x + 1]
                         , y' <- [y     .. y + 1]
                         , not (x' == x && y' == y)
                         ]

    ixS a (x, y)
      = ixa a [ (x', y') | x' <- [x - 1 .. x + 1]
                         , y' <- [y - 1 .. y    ]
                         , not (x' == x && y' == y)
                         ]

    ixE a (x, y)
      = ixa a [ (x', y') | x' <- [x - 1 .. x    ]
                         , y' <- [y - 1 .. y + 1]
                         , not (x' == x && y' == y)
                         ]

    ixNW a (x, y)
      = ixa a [ (x', y') | x' <- [x     .. x + 1]
                         , y' <- [y     .. y + 1]
                         , not (x' == x && y' == y)
                         ]

    ixNE a (x, y)
      = ixa a [ (x', y') | x' <- [x - 1 .. x    ]
                         , y' <- [y     .. y + 1]
                         , not (x' == x && y' == y)
                         ]

    ixSW a (x, y)
      = ixa a [ (x', y') | x' <- [x     .. x + 1]
                         , y' <- [y - 1 .. y    ]
                         , not (x' == x && y' == y)
                         ]

    ixSE a (x, y)
      = ixa a [ (x', y') | x' <- [x - 1 .. x    ]
                         , y' <- [y - 1 .. y    ]
                         , not (x' == x && y' == y)
                         ]

-- ----------------------------------------

cntWood :: Coll -> (Int, Int)
cntWood = foldl' cnt (0,0) . elems
  where
    cnt acc@(t, l) c
      | c == trees      = (t + 1, l)
      | c == lumberYard = (t, l + 1)
      | otherwise       = acc


iter :: (a -> a) -> (a -> b) -> a -> [b]
iter f g = go
  where
    go s = g s : go (f s)

-- ----------------------------------------
--
-- nn longer used, a better solution imported
-- from Data.List.Period
--
-- this function does not work in general for detecting periods,
-- it only works, if the period contains at least 1 element,
-- that occurs only once.
--
-- In this game of life like puzzle gen i only depends on
-- gen i-1, so if there is a gen, that occurs 2 times, we've
-- found the period, it's the part from the 1. occurence to the
-- 2. one

findPeriod :: Eq a => [a] -> Maybe ([a], [a])
findPeriod = go []
  where
    go _  []                  = Nothing
    go ls xs@(x1 : xs1)
      | Just (px, ps) <- findDup xs
      , checkPeriod px ps xs = Just (reverse ls ++ px, ps)
      | otherwise            = go (x1 : ls) xs1

checkPeriod :: Eq a => [a] -> [a] -> [a] -> Bool
checkPeriod px ps xs =
  px `isPrefixOf` xs
  &&
  isPeriod (drop (length px) xs)
  where
    lenPs = length ps

    isPeriod xs' =
      xs' `isPrefixOf` ps
      ||
      ( ps `isPrefixOf` xs'
        &&
        isPeriod (drop lenPs xs')
      )

findDup :: Eq a => [a] -> Maybe ([a], [a])
findDup = go []
  where
    go _  []                     = Nothing
    go ls (x : xs)
      | Just res <- lookup' x ls = Just (reverse ls, res)
      | otherwise                = go (x : ls) xs

lookup' :: Eq a => a -> [a] -> Maybe [a]
lookup' y = go []
  where
    go _   []        = Nothing
    go acc (x : xs)
      | x == y       = Just (x : acc)
      | otherwise    = go (x : acc) xs

-- ----------------------------------------

solve1 :: Int -> Input -> Int
solve1 gen css = trace ("initial state:\n\n" ++ showBoard coll) $
             trace ("\nfinal state:\n\n" ++ showBoard res ) $
             cntLumber * cntTrees
  where
    coll = toColl css
    carr = cArr (bounds coll)

    act  = foldr (.) id $ replicate gen (nextGen carr)
    res  = act coll

    (cntLumber, cntTrees) = cntWood res

-- the dev version

i'Round :: Int -> ([(Int, Int)], [(Int, Int)]) -> (Int, Int)
i'Round i (px, ps)
  | i' < 0    = px !! i
  | otherwise = ps !! r
  where
    i' = i - length px
    r  = i' `mod` length ps

-- after a few 100 generation the board runs into a cycle of a few 10 generations
-- for our input about 600 rounds are sufficient
--
-- n is the number of generations really computed,
-- all theses generations are computed and the #s of trees and lumber are
-- collected in a sequence, this sequence is analysed for a period
-- if a period is found, result for arbitrarily large # of rounds
-- can be computed by simple modulo operation and an acces into the
-- list for the period
--
-- rounds is the maybe very large # generation, e.g. 1,000,000,000

solve2 :: Int -> Int -> Input -> Maybe Int
solve2 n rounds css =
  fmap (uncurry (*) . i'Round rounds) .
  L.findPeriod .
  take n .
  iter (nextGen carr) cntWood
  $ coll
  where
    coll = toColl css
    carr = cArr (bounds coll)

solve' n rounds css =
  fmap (uncurry (*) . i'Round rounds) .
  L.findPeriod .
  take n .
  iter (nextGen carr) cntWood
  $ coll
  where
    coll = toColl css
    carr = cArr (bounds coll)

-- ----------------------------------------

type Input = [[Char]]

toColl :: [[Char]] -> Coll
toColl css
  | all (== dimY) dimXs = array ((0,0), (dim', dim')) ixcss

  | otherwise =
      error "toColl: input not a square board"
  where
    ixcss :: [(Point, Acre)]
    ixcss = concat $ zipWith line' [0..dim'] css
      where
        line' y cs = zipWith char' [0..dim'] cs
          where
            char' x c = ((x, y), c)

    dim'  = dimY - 1
    dimY  = length css
    dimXs = map length css


fromString :: String -> Input
fromString = lines

showBoard :: Coll -> String
showBoard col =
  unlines colls
  where
    ((x0, y0), (x1, y1)) = bounds col
    line y = [col ! (x, y) | x <- [x0 .. x1]]
    colls  = [line y       | y <- [y0 .. y1]]

-- ----------------------------------------

ex :: String
ex = unlines $
  [ ".#.#...|#."
  , ".....#|##|"
  , ".|..|...#."
  , "..|#.....#"
  , "#.#|||#|#|"
  , "...#.||..."
  , ".|....|..."
  , "||...#|.#|"
  , "|.||||..|."
  , "...#.|..|."
  ]

ex2 :: String
ex2 = unlines $
  [
  ]

inp' :: IO String
inp' = readFile "Year18/Day18/day18.txt"

inp :: String
inp = "#...|.|.|....##.##....|.#...|.|.|......#....|#...#\n#..#|.#...||#...|.#..#...#......|.|#..#.##....#|#.\n.#..##...#|.|..#|....|..#|.|#.#|.......#..#...|#..\n.|.|#..|.#....##......#..#.#...|....|#...|#.......\n#....|#.#.|..|..#.....#|.#.||.#.|.....#..#..#.....\n.#||##..#..|.|....#.|....#.#..|.....#.#|........|.\n.|###|#.|..##|#|...#|||.|..|.|.|#|.#.#|.#...#.|.##\n..||#..##.||..|...|..|.||.|#...|..|..#....###.#...\n..|......##.....#.###......#.#.......|...|.#|...#|\n#..|...|.|#|.|....|#.......|.#.....#|.#.....#..#.#\n.#.|..#..#|##..|||..##............#..|..#..|...|.#\n.##.#|....|..#..|.#.|....|.||#...|..#||......|#...\n.....#....#.#.#.#.||.....#.##|..#...#.|......#....\n##.#|.#...#.|..|.|....#|#.....#.|.###|#....#|||...\n......|..#......#..|...|..#|##..#|.......||.|....|\n..#......#.|.|#....##.##.#.|.|..|##..###....#||.|#\n.#|.....#...#..#.||.#||.##...||....|.......|#...##\n.......||..|#.|##...#.|.#..|..|.#.|####|.||...|#|.\n#...#.#..|##..|#..|.#.|#.|##..|..|#....#.|.....#..\n|....#..#.....|..##..#..|.|#..||#|#...||..#|..|...\n........##......|......|......||.......##.|#.||..|\n|..#.||.#.#..#..|||....|#|..|.#|...|..|.#.||.|.|.|\n.#...|..|.|#.......|#.|......|...#|||....#..|...|#\n...###|...#..|..#.|...|#|.....##.....|.##.||.|.##|\n#..##.|.||#.#....#|.....|#..||...#.||.##|.#..|..|.\n|....|#.....|...#..|..|..#..#|........||..#.|.....\n..#......|..#....#.|....#.|###|.##|.|...#.#...||..\n..#.|#....|...#||.|.#...#..#...#..#.|.#|.||..|#..|\n....|.|#..|...||..||#.|..#||.|#..#.#....#.|.....#|\n#|.|..|..#......|#|||.##.#......#.|#..#.|.##.#|..#\n..#|......##.|.#...|.#...#....#.###|#......|......\n..#.#.#|...#|....|...#.##...##...#..#|..|#.|###...\n|||.#..###.|......|..#.....#...|#.|.|...#...#|..#|\n##...#.#|#.|....||....|.....#|....#....|..|....#.#\n.........###.#.#..##..|##..#|...#...|...#|###.|..|\n.|#.........##.....#.##.|#.#....#....#|....#....#.\n.|....#...#||.|.......|.#..#.|..|#...#|.....|...#.\n.|#..|||....#..|#|.|.#.||..|.#|.#|#|||.|#||...#.#.\n#.#|.|.#...#.#.|||..|...#..#.##....#.#.#|.||....##\n.|.|....##..|#|.#...|#.|.|...#..#|#....|#.|.##.##.\n...#....#.....#..#...#..#.|#..#.|#.|..#..#.....|..\n#...#..|..###|....#.|...##|...#|...#.#....##...#.#\n.#.##..#.#.......................|..#|..##.|.|....\n|..#|#..|#....#...#....|..##..#||#..#.#.#.|#|.||##\n#.#...#.#...##.||...||....#...##|#|....|||#..|.|.|\n....#....|.|.....#|#...#..#|#....|#.|.#|.|..#....|\n|#.......||.#.|..|......##.||.....||.|..|....||#..\n..|#.....|...##.##..#|##|#....####..#|.......#.|..\n||#.#.|.##..#.|....#.||###..####||.#||...##.#..#|#\n.|....#.....#.....##.#..|#...|||.|....|#..|#|...#.\n"

-- ----------------------------------------
