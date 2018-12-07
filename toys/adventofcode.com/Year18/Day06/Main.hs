{-# LANGUAGE BangPatterns #-}

-- solution for
-- http://adventofcode.com/2018/day/6


module Main where

import           Data.List
import qualified Data.Map.Strict as M
import           Util.Main1 (main12)
-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-06"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 10000 . fromString

type Points       = [Point]
type Point        = (Int, Int)
type BBox         = (Point, Point)
type NeighbourMap = M.Map Point Point
type RegionSizes  = M.Map Point Int

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

norm :: Point -> Int
norm = dist (0, 0)

maxP :: Point -> Point -> Point
maxP (x1, y1) (x2, y2) = (x1 `max` x2, y1 `max` y2)

minP :: Point -> Point -> Point
minP (x1, y1) (x2, y2) = (x1 `min` x2, y1 `min` y2)

minimumP :: Points -> Point
minimumP = foldl1 minP

maximumP :: Points -> Point
maximumP = foldl1 maxP

nearestP :: Points -> Point -> Maybe (Int, Point)
nearestP qs p =
  case dists of
    (dp1@(d1, _p1) : (d2, _p2) : _)
      | d1 < d2
        -> Just dp1
    _   -> Nothing
  where
    dists = sort $
            [ (dist q p ,q)
            | q <- qs
            ]

bbox :: Points -> BBox
bbox ps = (minimumP ps, maximumP ps)

buildNeighbourMap :: BBox -> Points -> NeighbourMap
buildNeighbourMap ((minX, minY), (maxX, maxY)) ps =
  M.fromList pps
  where
    pps = [ (q, p)
          | q <- [ (x, y)
                 | x <- [minX - 1 .. maxX + 1]
                 , y <- [minY - 1 .. maxY + 1]
                 ]
          -- , q `notElem` ps
          , Just (_d, p) <- [nearestP ps q]
          ]

restrictToBorder :: BBox -> NeighbourMap -> NeighbourMap
restrictToBorder ((minX, minY), (maxX, maxY)) =
  M.foldlWithKey remBorder M.empty
  where
    remBorder m' k@(x, y) v
      | x == minX || x == maxX
        ||
        y == minY || y == maxY = M.insert k v m'
      | otherwise              = m'

infiniteP :: NeighbourMap -> Points
infiniteP = nub . M.elems

countRegions :: NeighbourMap -> RegionSizes
countRegions =
  M.foldl' countR M.empty
  where
    countR m p = M.insertWith (+) p 1 m

solve1 :: Points -> Int
solve1 ps0 = maximum rsz
  where
    ps  = sort ps0
    box = bbox ps
    nm  = buildNeighbourMap box ps
    bm  = restrictToBorder  box nm
    inf = sort $ infiniteP bm
    inn = ps \\ inf
    rss = countRegions nm
    rsz = map (\ p -> maybe 0 id $ M.lookup p rss) inn

-- ----------------------------------------

sumDist :: Points -> Point -> Int
sumDist ps p = sum . map (dist p) $ ps

sumDistLT :: Int -> Points -> Point -> Bool
sumDistLT limit ps p = go 0 $ map (dist p) ps
  where
    go _acc []       = True
    go  acc (x : xs)
      | acc' < limit = go acc' xs
      | otherwise    = False
      where
        acc' = acc + x

pointsLT :: Int -> Points -> Points -> Points
pointsLT limit ps = filter (sumDistLT limit ps)

expandBB :: BBox -> BBox
expandBB ((minX, minY), (maxX, maxY)) =
  ((minX - 1, minY - 1), (maxX + 1, maxY + 1))

bboxIxs :: BBox -> Points
bboxIxs ((minX, minY), (maxX, maxY)) =
  [ (x, y)
  | x <- [minX .. maxX]
  , y <- [minY .. maxY]
  ]

borderIxs :: BBox -> Points
borderIxs ((minX, minY), (maxX, maxY)) =
  [ (x, minY) | x <- [minX .. maxX] ]
  ++
  [ (x, maxY) | x <- [minX .. maxX] ]
  ++
  [ (minX, y) | y <- [minY + 1 .. maxY - 1] ]
  ++
  [ (maxX, y) | y <- [minY + 1 .. maxY - 1] ]

solve2 :: Int -> Points -> Int
solve2 limit ps =
  expand bbx cntBB
  where
    bbx   = bbox ps
    cntBB = length . pointsLT limit ps . bboxIxs $ bbx

    expand :: BBox -> Int -> Int
    expand bx sz
      | sz' == sz =            sz'  -- no point on the border contains to area
      | otherwise = -- trace (show bx') $
                    expand bx' sz'  -- expand bbox, does not occur with puzzle input
      where
        bx' = expandBB bx
        sz' = sz + (length . pointsLT limit ps $ borderIxs bx')

-- ----------------------------------------

fromString :: String -> Points
fromString = map (toPair . map read . words . filter (/= ',')) . lines
  where
    toPair [x,y] = (x, y)

ps1, ps2 :: Points
ps1 = fromString ex
ps2 = fromString inp

bx1 = bbox ps1
bx2 = bbox ps2

-- ----------------------------------------

ex :: String
ex = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"

inp' :: IO String
inp' = readFile "Year18/Day06/day06.txt"

inp :: String
inp = "195, 221\n132, 132\n333, 192\n75, 354\n162, 227\n150, 108\n46, 40\n209, 92\n153, 341\n83, 128\n256, 295\n311, 114\n310, 237\n99, 240\n180, 337\n332, 176\n212, 183\n84, 61\n275, 341\n155, 89\n169, 208\n105, 78\n151, 318\n92, 74\n146, 303\n184, 224\n285, 348\n138, 163\n216, 61\n277, 270\n130, 155\n297, 102\n197, 217\n72, 276\n299, 89\n357, 234\n136, 342\n346, 221\n110, 188\n82, 183\n271, 210\n46, 198\n240, 286\n128, 95\n111, 309\n108, 54\n258, 305\n241, 157\n117, 162\n96, 301\n"
