{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Figure
  ( module Figure
  , Path
  )
where

import Data.Board
import Algorithms.AStar
import Data.PriorityQueue.Heap

import Control.Lens

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
-- import qualified Data.List       as L

-- --------------------

type Figure    = Board Color
data Color     = G | R | W | Y
type Pos       = Int
type Solution  = [Pos]
type Solutions = [Solution]
type Game a    = (Int, a, Solutions)
type GameF     = Game Figure
type GameS     = Game String

-- --------------------
--
-- Color instances

deriving instance Eq   Color
deriving instance Ord  Color
deriving instance Enum Color
deriving instance Show Color
deriving instance Read Color

-- --------------------

validateBoard :: Int -> Int -> Figure -> Either String Figure
validateBoard w h b@(Board m)
  | out       = Left $ "there are tiles outside the "
                       ++
                       show w ++ "x" ++ show h
                       ++
                       " sized board"
  | hole      = Left   "board not completely filled with tiles"
  | otherwise = Right b

  where
    out = any off $ M.keys m
      where
        off (V2 x y) =
          x <= 0 || x > w
          ||
          y <= 0 || y > h

    hole = M.size m < w * h

-- --------------------

-- the possible next moves
-- the leftmost coordinate represents clusters of tiles

nextMoves' :: PartBoard Color -> [Pos]
nextMoves' = S.toAscList
             . S.map (^. _1)
             . S.filter ((== 1) . (^. _2))
             . clusterReps
             . clusters

bottomClusters :: PartBoard Color -> PartCoords
bottomClusters = S.filter row1 . clusters
  where
    -- search for a Coord with y == 1
    row1 :: Coords -> Bool
    row1 = S.foldr f1 False
      where
        f1 (V2 _x y) acc = y == 1 || acc

-- maybe used as a heuristic, how many steps maybe used
-- to clear the board

numClusters :: PartBoard Color -> Int
numClusters = M.foldl' f1 0
  where
    f1 acc s = acc + S.size s

nextBoards :: Figure -> [(Pos, Figure)]
nextBoards b = S.foldr' move [] cs
  where
    pb = partBoard . invertBoard $ b
    cs = bottomClusters pb

    move c acc = (m', b') : acc
      where
        m' = minCoord c ^. _1
        b' = removeAndDrop c b

-- estimated cost (# of moves) until board is cleared

numberOfClusters :: Figure -> Int
numberOfClusters = S.size . clusters . partBoard . invertBoard

-- final state reached
nullBoard :: Figure -> Bool
nullBoard (Board b) = M.null b

playFigure :: Figure -> Path Pos -> [(Int, Pos, Figure)]
playFigure b0 p0 = scanl step (0, 0, b0) (zip [1..] p0)
  where
    step :: (Int, Pos, Figure) -> (Int, Pos) -> (Int, Pos, Figure)
    step (_, _, b) (i, p) = (i, p, b')
      where
        b' = snd . head . filter ((== p) . fst) . nextBoards $ b

-- --------------------

instance AStar Figure where
  type Move Figure = Pos

  nextMoves :: Figure -> [(Pos, Figure)]
  nextMoves = nextBoards

  -- all tiles removed from board?
  finalState :: Figure -> Bool
  finalState = nullBoard

  -- all moves cost the same, we need the shortest path
  moveCost :: Pos -> Figure -> Cost
  moveCost _mv _b = 1

  -- heuristic: every cluster needs one move to be discarded
  estimatedCost :: Figure -> Cost
  estimatedCost b = toEnum $  numberOfClusters b


initBoardAStar :: Figure -> AStarState Figure Pos
initBoardAStar b = (initAStar b) { _weightCost = 0.75, _smax = 0 }

-- # moves used needs higher weight than # of clusters
-- else search isn't broad enough
--
-- @0.50 * pathCost + (1.0 - 0.50) * heuristics@ does not work
--
-- @0.66 * pathCost + (1.0 - 0.66) * heuristics@
-- worked fine for all puzzles solved until figure97

-- _weightCost = 0.66 (2/3) is too small, 0.75 (3/4) worked
-- figure98 solvable in 10 move, but solution with 11 moves found
-- 0.66:  885 moves tried, 11 moves
-- 0.75: 4009 moves tried, 10 moves (required)

-- _weightCost = 0.75 (3/4) did work until figure144
-- #144: (see comment at end of Main.hs)
-- 0.75: 41756 moves tried
--       42712 moves not tried, 11 moves (10 required)
-- 0.80: 46027 moves tried
--       58077 moves not tried, 10 moves [3,3,3,4,2,5,1,1,1,1]


solveBoard :: Figure -> ([Pos], Int, Int)
solveBoard b =
  ( mvs
  , _scnt s1
  , sizeQ (_openStates s1)
  )
  where
    s0        = initBoardAStar b
    (res, s1) = aStar s0
    mvs       = case res of
      Nothing                -> []
      Just Nothing           -> []
      Just (Just (_, ms, _)) -> reverse ms

-- --------------------
