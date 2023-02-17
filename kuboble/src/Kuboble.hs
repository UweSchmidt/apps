{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE LambdaCase         #-}

-- https://kuboble.com/
--
-- kuboble puzzle solver
-- implemented with AStar algorithm
-- but used as a breadth firs search
-- due to missing a good heuristic

module Kuboble
  ( module Kuboble
  , Path
  )
where

import           Algorithms.AStar
import           Data.Board
import           Data.PriorityQueue.Heap

import           Control.Lens
import           Control.Monad.Except
import           Data.Maybe ()
import           Text.Printf ()

import qualified Data.Map.Strict         as M ()
import qualified Data.Set                as S
import qualified Data.List               as L

-- --------------------

data Kuboble2 = K2 { _tiles :: !Kuboble
                   , _balls :: !Kuboble
                   }

type Kuboble   = Board Boble

data Boble     = Black           -- the tiles on a board
               | White
               | Target  !RGB
               | Ball    !RGB

data RGB       = Red             -- the colors of the balls and target tiles
               | Green
               | Blue

data Dir       = Down | Up | Left_ | Right_
type Step1     = ((Coord, Coord), Dir)
type Move1     = (RGB, Step1)

type Solution  = [Move1]

type Solutions = [Solution]

type Game a    = (Int, a, Solutions)
type GameK     = Game Kuboble
type GameS     = Game String

-- --------------------

deriving instance Eq   RGB
deriving instance Ord  RGB
deriving instance Enum RGB
deriving instance Show RGB
deriving instance Read RGB

deriving instance Eq   Dir
deriving instance Ord  Dir
deriving instance Enum Dir
deriving instance Show Dir
deriving instance Read Dir

deriving instance Eq   Boble
deriving instance Ord  Boble
deriving instance Show Boble

instance Semigroup Boble where
  Black <> x = x
  x     <> _ = x

instance Monoid Boble where
  mempty = Black

deriving instance Show Kuboble2

instance Eq Kuboble2 where            -- during a match only
  b1 == b2 = _balls b1 == _balls b2   -- the balls are changed
                                      -- for a state compare can be
                                      -- done by comparing the balls

instance Ord Kuboble2 where
  compare k1 k2 = compare (_balls k1) (_balls k2)

-- --------------------
--
-- basic optics

target :: Prism' Boble RGB
target = prism'
  Target
  (\case
      Target c -> Just c
      _        -> Nothing
  )

ball :: Prism' Boble RGB
ball =
  prism'
  Ball
  ( \case
      Ball c -> Just c
      _      -> Nothing
  )

white :: Prism' Boble ()
white =
  prism'
    (const White)
    ( \case
        White -> Just ()
        _     -> Nothing
    )

black :: Prism' Boble ()
black =
  prism'
    (const Black)
    ( \case
        Black -> Just ()
        _ -> Nothing
    )


-- --------------------

legalKubole2 :: Int -> Int -> Kuboble2 -> Either String Kuboble2
legalKubole2 w h k@(K2 tiles' balls') = do
  _ <- legalCoords w h tiles'
  _ <- legalCoords w h balls'
  bls <- onlyBalls balls'
  tls <- onlyTiles tiles'
  unless (bls /= tls) $
    throwError "balls and targets differ"
  return k

onlyBalls :: Kuboble -> Either String [RGB]
onlyBalls k
  | noOfCoords bls /= noOfCoords k =
      throwError "wrong tiles in balls map"
  | null bcs =
      throwError "no balls in balls map"
  | length bcs /= noOfCoords bls =
      throwError "balls with same color found"
  | otherwise =
      return bcs
  where
    bls = balls k
    bcs = ballColors bls

onlyTiles :: Kuboble -> Either String [RGB]
onlyTiles k
  | noOfCoords tls /= noOfCoords k =
      throwError "balls found in tile map"
  | null tcs =
      throwError "no target tiles found"
  | length tcs /= noOfCoords tgts =
      throwError "targets with same color"
  | otherwise =
      return tcs
  where
    tls  = filterBoard
           (\case {Ball _ -> False; _ -> True}) k
    tgts = targets k
    tcs  = targetColors tgts


legalCoords :: Int -> Int -> Kuboble -> Either String Kuboble
legalCoords w h b
  | out       = throwError $
                "there are tiles outside the "
                ++
                show w ++ "x" ++ show h
                ++
                " sized board"
  | otherwise = return b

  where
    out = any off $ toCoords b
      where
        off (V2 x y) =
          x <= 0 || x > w
          ||
          y <= 0 || y > h

-- ----------------------------------------

targets,
  balls,
  noBalls :: Kuboble -> Kuboble

targets   = filterBoard (has target)
balls     = filterBoard (has ball)
noBalls   = filterBoard (not . has ball)

targetColors :: Kuboble -> [RGB]
targetColors =
  L.sort . concatMap (^.. target) . tiles . targets

ballColors :: Kuboble -> [RGB]
ballColors =
  L.sort . concatMap (^.. target) . tiles . balls

ballsOnBoard :: Kuboble2 -> Kuboble
ballsOnBoard (K2 bo ba) = ba <> bo

shiftOrgKuboble :: Kuboble -> Kuboble
shiftOrgKuboble b =
  shiftBoard (V2 1 1 - fst (bboxBoard b)) b

shiftOrgKuboble2 :: Kuboble2 -> Kuboble2
shiftOrgKuboble2 k2@(K2 bo ba) =
  K2 bo' ba'
  where
    delta = V2 1 1 - fst (bboxBoard $ ballsOnBoard k2)
    bo' = shiftBoard delta bo
    ba' = shiftBoard delta ba

-- ----------------------------------------

dirs :: [(Dir, Coord)]            -- the 4 move dirs
dirs = [ (Down,   V2 0 (-1))
       , (Up,     V2 0   1 )
       , (Left_,  V2 (-1) 0)
       , (Right_, V2   1  0)
       ]

steps :: Kuboble -> Coord -> [Step1]
steps b c0 = concatMap f dirs
  where
    f dd = step' dd b c0

step' :: (Dir, Coord) -> Kuboble -> Coord -> [Step1]
step' (dir, delta) b c0
  | c0 == c1  = []
  | otherwise = [((c0, c1), dir)]
  where
    c1 = step'' delta b c0

step'' :: Coord -> Kuboble -> Coord -> Coord
step'' delta b = go
  where
    go c0
      | (||) <$> has white <*> has target $ t1 = go c1
      | otherwise                              =                c0
      where
        c1 = c0 + delta
        t1 = b ^. theBoardAt c1

moveBall :: Coord -> Coord -> Kuboble2 -> Kuboble2
moveBall c0 c1 k2 =
  k2 {_balls = mv (_balls k2)}
  where
    mv b = b & theBoardAt c0 .~ mempty
             & theBoardAt c1 .~ (b ^. theBoardAt c0)


solved :: Kuboble2 -> Bool
solved (K2 tiles' balls') = foldrBoard eq' True balls'
  where
    eq' c v res =
      match (tiles' ^? theBoardAt c . target) (v ^? ball)
      &&
      res
      where
        match (Just c1) (Just c2) = c1 == c2
        match  _         _        = False

nextMovesK2' :: Kuboble2 -> [Move1]
nextMovesK2' k2 =
  foldlBoard mvs [] $ _balls k2
  where
    b2 = ballsOnBoard k2

    mvs res c0 v
      | Just c <- v ^? ball = ms c <> res
      | otherwise           =         res
      where
        ms c' = map (c',) $ steps b2 c0

nextMovesK2 :: Kuboble2 -> [(Move1, Kuboble2)]
nextMovesK2 k2 =
  map mv $ nextMovesK2' k2
  where
    mv m@(_rgb, ((c0, c1), _dir)) =
      (m, moveBall c0 c1 k2)


instance AStar Kuboble2 where
  type Move Kuboble2 = Move1

  nextMoves :: Kuboble2 -> [(Move1, Kuboble2)]
  nextMoves = nextMovesK2

  -- all balls on target tiles
  finalState :: Kuboble2 -> Bool
  finalState = solved

  -- all moves cost the same, we need the shortest path
  moveCost :: Move1 -> Kuboble2 -> Cost
  moveCost _mv _k2 = 1

  -- no heuristic, cost constantly 0
  estimatedCost :: Kuboble2 -> Cost
  estimatedCost = const 0

initK2AStar :: Kuboble2 -> AStarState Kuboble2 Move1
initK2AStar k2 = (initAStar k2) {_weightCost = 1.0, _smax = 0}

solveKuboble :: Kuboble2 -> (Solution, (Int, Int, Int))
solveKuboble k2 =
  ( mvs
  , (_scnt s1, S.size (_closedStates s1), sizeQ (_openStates s1))
  )
  where
    s0        = initK2AStar k2
    (res, s1) = aStar s0
    mvs       = case res of
      Nothing                -> []
      Just Nothing           -> []
      Just (Just (_, ms, _)) -> reverse ms

-- ----------------------------------------
