{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Board where

import Data.Maybe (fromMaybe)
import Data.Char (toUpper)
import Data.List (intercalate, foldl')
import Data.Map.Strict (Map)
import Data.Set (Set, member, disjoint, union, toList, toAscList, fromList)

-- import Linear.V2

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

-- --------------------

data Point a = P {_xxx :: !a, _yyy :: !a}

deriving instance Functor Point

instance Applicative Point where
  P fx fy <*> P x y = P (fx x) (fy y)
  pure x = P x x

deriving instance Eq   a => Eq (Point a)

instance Ord a => Ord (Point a) where
  P x1 y1 <= P x2 y2 = y1 < y2 || y1 == y2 && x1 <= x2

instance Show a => Show (Point a) where
  show (P x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- --------------------

emptySet :: Set a
emptySet = S.empty

set1 :: a -> Set a
set1 = S.singleton

add1 :: Ord a => a -> Set a -> Set a
add1 = S.insert

nullSet :: Set a -> Bool
nullSet = S.null

intersect :: Ord a => Set a -> Set a -> Set a
intersect = S.intersection

transf :: Ord a => (a -> a) -> Set a -> Set a
transf = S.map

showSet :: Show a => Set a -> String
showSet =
  ("{" ++) . (++ "}") . intercalate "," . map show . toAscList

-- --------------------

type Coord      = Point Int
type Coords     = Set Coord      -- set of coordinates
type PartCoords = Set Coords     -- set of set of connected coordinates

shiftX :: Int -> Coord -> Coord
shiftX d (P x y) = P (x + d) y

shiftY :: Int -> Coord -> Coord
shiftY d (P x y) = P x (y + d)

expand1 :: Coords -> Coords
expand1 s =
  transf (shiftX 1) s `union` transf (shiftX (negate 1)) s
  `union`
  transf (shiftY 1) s `union` transf (shiftY (negate 1)) s

connected :: Coords -> Coords -> Bool
connected s1 s2 =
  not $ disjoint (expand1 s1) s2

partConnected :: Coords -> PartCoords
partConnected =
  fromList . foldl' (flip unite) [] . map set1 . toAscList

unite :: Coords -> [Coords] -> [Coords]
unite s0 ps = unite' es0 s0 ps
  where
    es0 = expand1 s0

    unite' es s []         = [s]
    unite' es s (s1 : ps1)
      | disjoint es s1     = s1 : unite' es s ps1
      | otherwise          = unite (s `union` s1) ps1

-- --------------------

newtype Board a = Board (Map Coord a)

type InvBoard  a = Map a Coords
type PartBoard a = Map a PartCoords

deriving instance Functor Board
deriving instance Eq   a => Eq   (Board a)
deriving instance Ord  a => Ord  (Board a)
deriving instance Show a => Show (Board a)

invertBoard :: Ord a => Board a -> InvBoard a
invertBoard (Board m) =
  M.foldlWithKey' f M.empty m
  where
    f im c v = M.insertWith union v (set1 c) im

invert1Board :: InvBoard a -> Board a
invert1Board = Board . M.foldlWithKey' f1  M.empty
  where
    f1 acc v cs = S.foldl' f2 acc cs
      where
        f2 acc' co = M.insert co v acc'

partBoard :: InvBoard a -> PartBoard a
partBoard = fmap partConnected

part1Board :: PartBoard a -> InvBoard a
part1Board = fmap (S.foldl' union emptySet)

clusters :: PartBoard a -> PartCoords
clusters = M.foldl' union emptySet

clusterReps :: PartCoords -> Coords
clusterReps = S.map S.findMin

dropDown :: Coords -> (Coord -> Coord)
dropDown = foldl' f1 id . toAscList
  where
    f1 f c = f . drop c

    drop :: Coord -> (Coord -> Coord)
    drop (P dx dy) c@(P x y)
      | dx == x
        &&
        dy <  y   = P x (y - 1)
      | otherwise = c

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

type Figure = Board Color

data Color = G | R | W | Y

type Pos     = Int
type Quality = Int

deriving instance Eq   Color
deriving instance Ord  Color
deriving instance Enum Color
deriving instance Show Color
deriving instance Read Color

-- the possible next moves
-- the leftmost coordinate represents clusters of tiles

nextMoves' :: PartBoard Color -> [Pos]
nextMoves' = S.toAscList
            . S.map _xxx
            . S.filter ((== 1) . _yyy)
            . clusterReps
            . clusters

bottomClusters :: PartBoard Color -> PartCoords
bottomClusters = S.filter row1 . clusters
  where
    row1 = (== 1) . _yyy . S.findMin

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
        m' = _xxx $ S.findMin c
        b' = removeAndDrop c b

-- estimated cost (# of moves) until board is cleared

numberOfClusters :: Figure -> Int
numberOfClusters = S.size . clusters . partBoard . invertBoard

-- final state reached
nullBoard :: Figure -> Bool
nullBoard (Board b) = M.null b

playFigure :: Figure -> Path Pos -> [(Int, Pos, Figure)]
playFigure b0 p0 = scanl step (0, 0, b0) (zip [1..] $ reverse p0)
  where
    step :: (Int, Pos, Figure) -> (Int, Pos) -> (Int, Pos, Figure)
    step (_, _, b) (i, p) = (i, p, b')
      where
        b' = snd . head . filter ((== p) . fst) . nextBoards $ b

-- --------------------

parseBoard :: String -> Figure
parseBoard css = Board . M.fromList . toBoardList . parse $ css
  where
    parse =
      map (map toColor . filter isColor) . reverse . lines
      where
        isColor = (`elem` ("GRWY" :: String))
        toColor = read . (:[]) . toUpper

    toBoardList :: [[Color]] -> [(Coord, Color)]
    toBoardList =
      concat . zipWith ln [1..]
      where
        ln y cs =
          concat $ zipWith co [1..] cs
          where
            co x c = [(P x y, c)]

printBoard :: Figure -> String
printBoard (Board m) = pr 5 5
  where
    pr w h =
      unlines $ reverse [ln i | i <- [1..h]]
      where
        ln y =
          unwords [toC j | j <- [1..w]]
          where
            toC x = fromMaybe "." . fmap show . M.lookup (P x y) $ m

printNextBoards :: [(Pos, Figure)] -> String
printNextBoards bs = intercalate "\n" . map f1 $ bs
  where
    f1 (m, b) =
      "move: " ++ show m ++ "\n"
      ++
      printBoard b
      ++
      "clusters: " ++ show (numberOfClusters b) ++ "\n"

printGame :: Figure -> Path Pos -> String
printGame b0 p0 = concatMap step $ playFigure b0 p0
  where
    step (i, p, b) =
      line1 ++ "\n"
      ++
      printBoard b
      ++
      "\n"
      where
        line1
          | i == 0 = "initial board:"
          | otherwise = show i ++ ". move: tile " ++ show p

-- --------------------

s1 :: String  -- figure #75
s1 = unlines $
  [ "R R R W W"
  , "G W W G R"
  , "W Y W Y G"
  , "R Y Y R R"
  , "R Y G G Y"
  ]

s2 :: String  -- figure #76, 11 moves
s2 = unlines $
  [ "R G W W R"
  , "W W W G Y"
  , "R R R Y W"
  , "Y R R Y R"
  , "G R Y G W"
  ]

s3 :: String  -- figure #78, 9 moves
s3 = unlines $
  [ "G Y W G Y"
  , "Y W W G Y"
  , "W G G W W"
  , "W G Y G R"
  , "W W G R G"
  ]

s4 :: String -- figure #77, 10 moves
s4 = unlines $
  [ "G Y G G W"
  , "Y W W G Y"
  , "Y R W Y W"
  , "G R Y Y W"
  , "R Y W R Y"
  ]

s5 :: String -- figure #77, 10 moves
s5 = unlines $
  [ "Y W G G R"
  , "Y R R G G"
  , "G Y G W W"
  , "W G G W G"
  , "W G W R W"
  ]


(b1, i1, p1, c1, n1, pb1, nbs1, ass1) = fff s1
(b2, i2, p2, c2, n2, pb2, nbs2, ass2) = fff s2
(b3, i3, p3, c3, n3, pb3, nbs3, ass3) = fff s3
(b4, i4, p4, c4, n4, pb4, nbs4, ass4) = fff s4
(b5, i5, p5, c5, n5, pb5, nbs5, ass5) = fff s5

fff s = (b, i, p, c, n, pb, nbs, ass)
  where
    b = parseBoard s
    i = invertBoard b
    p = partBoard i
    c = clusters p
    n = nextMoves' p
    pb = printBoard b
    nbs = nextBoards b
    ass = initBoardAStar b

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


  -- # moves used needs higher weight than # of clusters
  -- else search isn't broad enough
  -- 1 and 1.5 doeas not work, 2.0 worked in test cases

initBoardAStar :: Figure -> AStarState Figure Pos
initBoardAStar b = (initAStar b) { _weightCost = 2.0 }

-- --------------------

class AStar a where
  type Move a :: *

  nextMoves     :: a -> [(Move a, a)]
  finalState    :: a -> Bool
  moveCost      :: Move a -> a -> Cost
  estimatedCost :: a -> Cost


type Cost = Double

data AStarState a mv = AS
  { _closedStates :: Set a                -- set of processed states
  , _openStates   :: PQueue Cost (PSC a mv) -- set of states to be processed
  , _scnt         :: Int                  -- step counter
  , _smax         :: Int                  -- max # of steps
  , _weightCost   :: Cost                 -- weight of cost/estimatedCost
  }                                       -- 2.0: path cost are twice as
                                          -- importand as extimated distance
                                          -- to final state

type Path mv  = [mv]                      -- list of moves in reverse order

type PSC a mv = (a, Path mv, Cost)         -- state, path to state, total cost


deriving instance (Show a, Show mv) => Show (AStarState a mv)

initAStar :: a -> AStarState a mv         -- initial search state
initAStar b = AS emptySet q0 1 0 1.0      -- open states contain the start state
  where
    q0 = insertQ 0 (b, [], 0) emptyQ

aStar :: forall a mv.
         (AStar a, mv ~ Move a, Ord a)
      => AStarState a mv
      -> ( Maybe (Maybe (PSC a mv))
         , (AStarState a mv)
         )
aStar s@(AS cls opn cnt mx cw)
  | nullQ opn    = (Just Nothing,    s)           -- failure: no solution
  | finalState b = (Just $ Just pbc, s)           -- success
  | otherwise    = go (AS cls' opn'' cnt' mx cw)  -- continue
  where
    cnt' = cnt + 1

    go
      | mx > 0
        &&
        cnt' `mod` mx == 0 = (Nothing,)   -- terminate when max steps reached
      | otherwise          = aStar        -- repeat

    (pbc@(b, p, c), opn') = splitMin opn  -- get best open state

    cls'   = add1 b cls                   -- close current state
    news   = nextMoves b                  -- get next possible states

    opn'' = foldl' ins opn' news          -- insert new next states into open

    ins acc (mv', b')
      | b' `member` cls' = acc            -- ignore duplicates in open states
      | otherwise        = insertQ q' (b', mv' : p, c') acc
      where
        c' =      c  + moveCost  mv' b'   -- accumulate path costs
        q' = cw * c' + estimatedCost b'   -- quality is sum of path costs
                                          -- and estimated costs

-- --------------------
--
-- a very simple priority queue

newtype PQueue c a = PQ [(c, a)]

emptyQ :: PQueue c a
emptyQ = PQ []

nullQ :: PQueue c a -> Bool
nullQ (PQ q) = null q

sizeQ :: PQueue c a -> Int
sizeQ (PQ xs) = length xs

splitMin :: PQueue c a -> (a, PQueue c a)
splitMin (PQ (x : xs)) = (snd x, PQ xs)
splitMin _             = error "splitMin: queue empty"

minElem :: PQueue c a -> a
minElem = fst . splitMin

deleteMin :: PQueue c a -> PQueue c a
deleteMin = snd . splitMin

insertQ :: Ord c => c -> a -> PQueue c a -> PQueue c a
insertQ k v (PQ q) = PQ $ ins q
  where
    ins []                  = [(k, v)]
    ins qs@(q1@(k1, _v1) : qs1)
      | k > k1              = q1     : ins qs1
      | otherwise           = (k, v) : qs

deriving instance (Show c, Show a) => Show (PQueue c a)

-- --------------------
