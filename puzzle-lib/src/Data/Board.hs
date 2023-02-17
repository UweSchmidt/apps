{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Board
  ( module Data.Board
  , module Linear.V2
  )
where

import Data.Maybe
import Data.Map.Strict (Map)
import Data.Set        (Set)

import Control.Lens
import Linear.V2

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Data.List       as L

-- --------------------
--
-- Board data types
--
-- a game board
-- a mapping from coordinates to tiles
newtype Board a  = Board {_board :: Map Coord a}

-- coord is a 2-dim vector of Ints
type Coord       = V2 Int

-- set of coordinates
type Coords      = Set Coord

-- the inverted board: a mapping of tiles to coordinates
type InvBoard  a = Map a Coords

-- the inverted board, but the coordinates
-- are pratitioned into sets of connected coordinates
type PartBoard a = Map a PartCoords

-- the partitioned board
-- without the info about the tiles
type PartCoords = Set Coords     -- set of set of connected coordinates

-- --------------------
--
-- Board instances

deriving instance Functor Board
deriving instance Eq   a => Eq   (Board a)
deriving instance Ord  a => Ord  (Board a)
deriving instance Show a => Show (Board a)

instance Semigroup a => Semigroup (Board a) where
  Board m1 <> Board m2 =
    Board $ M.unionWith (<>) m1 m2

instance Semigroup a => Monoid (Board a) where
  mempty = Board M.empty
  {-# INLINE mempty  #-}

-- --------------------
--
-- basic optics

pair :: Iso' (V2 a) (a, a)
pair = iso (\ (V2 x y) -> (x, y)) (uncurry V2)
{-# INLINE pair  #-}

board :: Iso' (Board a) (Map Coord a)
board = iso _board Board
{-# INLINE board  #-}

theBoardAt :: (Eq a, Monoid a) => Coord -> Lens' (Board a) a
theBoardAt p = lens (boardAt p) (flip $ setBoardAt p)
{-# INLINE theBoardAt  #-}

isoBoardList :: (Eq a, Monoid a) => Iso' (Board a) [(Coord, a)]
isoBoardList = iso boardToList boardFromList
{-# INLINE isoBoardList  #-}

invertedBoard :: (Ord a) => Iso' (Board a) (InvBoard a)
invertedBoard = iso invertBoard invert1Board
{-# INLINE invertedBoard #-}

shiftedBoard :: Coord -> Iso' (Board a) (Board a)
shiftedBoard delta = iso (shiftBoard delta) (shiftBoard (negate delta))
{-# INLINE shiftedBoard  #-}

-- --------------------
--
-- basic Board ops

nullBoard :: Board a -> Bool
nullBoard = M.null . _board
{-# INLINE nullBoard  #-}

noOfCoords :: Board a -> Int
noOfCoords = M.size . _board
{-# INLINE noOfCoords  #-}

coords :: Board a -> [Coord]
coords = M.keys . _board
{-# INLINE coords  #-}

toCoords :: Board a -> Coords
toCoords = S.fromList . coords
{-# INLINE toCoords  #-}

tiles :: Board a -> [a]
tiles = M.elems . _board
{-# INLINE tiles  #-}

boardToList :: Board a -> [(Coord, a)]
boardToList = M.toAscList . _board
{-# INLINE boardToList  #-}

boardFromList :: (Eq a, Monoid a) => [(Coord, a)] -> Board a
boardFromList = L.foldl' ins mempty
  where
    ins m (p, x) = setBoardAt p x m
{-# INLINE boardFromList #-}

boardAt :: Monoid a => Coord -> Board a -> a
boardAt p b = fromMaybe mempty $ M.lookup p (_board b)
{-# INLINE boardAt  #-}

setBoardAt :: (Eq a, Monoid a) => Coord -> a -> Board a -> Board a
setBoardAt p x b
  | x == mempty = b & board %~ M.delete p
  | otherwise   = b & board %~ M.insert p x
{-# INLINE setBoardAt #-}

clearBoardAt :: (Eq a, Monoid a) => Coord -> Board a -> Board a
clearBoardAt p = setBoardAt p mempty
{-# INLINE clearBoardAt  #-}

filterBoard :: (a -> Bool) -> Board a -> Board a
filterBoard p b = b & board %~ M.filter p
{-# INLINE filterBoard  #-}

foldlBoard :: (r -> Coord -> a -> r) -> r -> Board a -> r
foldlBoard f r = M.foldlWithKey' f r . _board
{-# INLINE foldlBoard  #-}

foldrBoard :: (Coord -> a -> r -> r) -> r -> Board a -> r
foldrBoard f r = M.foldrWithKey f r . _board
{-# INLINE foldrBoard  #-}

shiftBoard :: Coord -> Board a -> Board a
shiftBoard delta = foldlBoard ins (Board M.empty)
  where
    ins b c v = b & board %~ M.insert (c + delta) v
{-# INLINE shiftBoard #-}

bboxBoard :: Board a -> (Coord, Coord)
bboxBoard b =
  case coords b of
    []       -> (V2 0 0, V2 0 0)
    (c1 : cs) -> L.foldl' mm (c1, c1) cs
  where
    mm (V2 xmi ymi, V2 xma yma) (V2 x y) =
      (V2 (xmi `min` x) (ymi `min` y), V2 (xma `max` x) (yma `max` y))

-- --------------------
--
-- partition coordinates into connected subsets

-- test whether sets of coordinates are connected
-- connected is defined as left or right or top and bottom
-- neighbours
-- e.g. (1,2) and (1,3), (1,1) and (2,1)

-- test: connected?
connected :: Coords -> Coords -> Bool
connected s1 s2 =
  not $ S.disjoint (expand1 s1) s2

-- part into connected subsets
partConnected :: Coords -> PartCoords
partConnected =
  S.fromList . L.foldl' (flip unite) [] . map S.singleton . S.toList
{-# INLINE partConnected #-}

unite :: Coords -> [Coords] -> [Coords]
unite s0 ps = unite' es0 s0 ps
  where
    es0 = expand1 s0

    unite' _  s []         = [s]
    unite' es s (s1 : ps1)
      | S.disjoint es s1   = s1 : unite' es s ps1
      | otherwise          = unite (s `S.union` s1) ps1

expand1 :: Coords -> Coords
expand1 s =
  S.map (shiftX 1) s `S.union` S.map (shiftX (negate 1)) s
  `S.union`
  S.map (shiftY 1) s `S.union` S.map (shiftY (negate 1)) s

shiftX :: Int -> Coord -> Coord
shiftX d (V2 x y) = V2 (x + d) y
{-# INLINE shiftX  #-}

shiftY :: Int -> Coord -> Coord
shiftY d (V2 x y) = V2 x (y + d)
{-# INLINE shiftY  #-}

compCoord :: Coord -> Coord -> Ordering
compCoord (V2 x1 y1) (V2 x2 y2) =
  case y1 `compare` y2 of
    EQ -> x1 `compare` x2
    c  -> c
{-# INLINE compCoord #-}

-- compare by y-coordinate first
minCoord :: Coords -> Coord
minCoord = head . L.sortBy compCoord . S.toList

-- --------------------
--
-- invert a Board into an InvBoard and back

invertBoard :: Ord a => Board a -> InvBoard a
invertBoard (Board m) =
  M.foldlWithKey' f M.empty m
  where
    f im c v = M.insertWith S.union v (S.singleton c) im

invert1Board :: InvBoard a -> Board a
invert1Board = Board . M.foldlWithKey' f1  M.empty
  where
    f1 acc v cs = S.foldl' f2 acc cs
      where
        f2 acc' co = M.insert co v acc'

-- transform an InvBoard into a partitioned board and back

partBoard :: InvBoard a -> PartBoard a
partBoard = fmap partConnected
{-# INLINE partBoard  #-}

part1Board :: PartBoard a -> InvBoard a
part1Board = fmap (S.foldl' S.union S.empty)
{-# INLINE part1Board  #-}

-- get the sets of connected coordinates
clusters :: PartBoard a -> PartCoords
clusters = M.foldl' S.union S.empty
{-# INLINE clusters  #-}

clusterReps :: PartCoords -> Coords
clusterReps = S.map minCoord -- S.findMin
{-# INLINE clusterReps  #-}

dropDown :: Coords -> (Coord -> Coord)
dropDown = L.foldl' f1 id . L.sortBy compCoord . S.toList
  where
    f1 f c = f . drop' c

    drop' :: Coord -> (Coord -> Coord)
    drop' (V2 dx dy) c@(V2 x y)
      | dx == x
        &&
        dy <  y   = V2 x (y - 1)
      | otherwise = c
{-# INLINE dropDown #-}

-- remove a set of tiles
-- and let the remaining tiles drop down

removeAndDrop :: Coords -> Board a -> Board a
removeAndDrop cs (Board m) = Board m''
  where
    ddf = dropDown cs                       -- the function for the new coords

    m'  = S.foldl' (flip M.delete) m cs     -- delete tiles
    m'' = M.foldlWithKey' f1 M.empty m'     -- drop down remaining tiles
      where
        f1 acc p' v' = M.insert (ddf p') v' acc

-- --------------------
