{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/18

module Main where

import           Data.Array.IArray
import           Data.Array.Unboxed (UArray)

import           Util.Main1 (main12)

import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  -- inp <- inp'
  main12 "2018-18"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

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
nextGen carr coll = listArray bds
  [ next (x, y) | x <- [x0..x1], y <- [y0..y1]]
  where
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

solve2 :: Input -> Int
solve2 = undefined

solve1 :: Input -> Int
solve1 css = trace ("initial state:\n\n" ++ showBoard coll) $
             trace ("\nfinal state:\n\n" ++ showBoard res ) $
             cntLumber * cntTrees
  where
    coll = toColl css
    carr = cArr (bounds coll)

    gen  = 10
    act  = foldr (.) id $ replicate gen (nextGen carr)
    res  = act coll

    cntLumber = length . filter (== lumberYard) . elems $ res
    cntTrees  = length . filter (== trees     ) . elems $ res

-- the dev version

solve' n css = (++ showRes) . showBoard . act $ coll
  where
    coll = toColl css
    carr = cArr (bounds coll)



    act  = foldr (.) id $ replicate n (nextGen carr)
    res  = act coll

    cntLumber = length . filter (== lumberYard) . elems $ res
    cntTrees  = length . filter (== trees     ) . elems $ res

    showRes   = "\n\nstate after " ++ show n ++ " generations: " ++
                "(lumber, trees) = " ++ show (cntLumber, cntTrees) ++
                " = " ++ show (cntLumber * cntTrees) ++ "\n\n"

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
