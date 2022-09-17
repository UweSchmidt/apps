-- A*-Algorithm for searching best paths
-- in a (game-) graph

module Algorithms.AStar
where

import Data.PriorityQueue.List   -- to be substituted by a more efficient impl.
import Data.Set                  (Set)

import qualified Data.Set        as S
import qualified Data.List       as L

import Data.Kind (Type)

-- --------------------

-- the requirements of a game state
-- needed by the A*-Algorithm

class AStar a where
  type Move a   :: Type

  nextMoves     :: a -> [(Move a, a)]     -- generator
  finalState    :: a -> Bool              -- success?
  moveCost      :: Move a -> a -> Cost    -- cost of a move
  estimatedCost :: a -> Cost              -- estimated distance
                                          -- from a final state

type Cost = Double

-- --------------------
--
-- the algorithm state during search

data AStarState a mv = AS
  { _closedStates :: Set a                -- set of processed states
  , _openStates   :: PQueue Cost (PSC a mv) -- set of states to be processed
  , _scnt         :: Int                  -- step counter
  , _smax         :: Int                  -- max # of steps
  , _weightCost   :: Cost                 -- weight of cost/estimatedCost
  }                                       -- range from 0.0 to 1.0
                                          -- 0.0: path costs are irrelevant
                                          -- 1.0: estimated cost is irrelevant
                                          -- 0.5: mean of both costs (default)
                                          -- 1.0 leads to breadth first search

type Path mv  = [mv]                      -- list of moves in reverse order

type PSC a mv = (a, Path mv, Cost)        -- state, path to state, total cost

-- --------------------
--
-- for debugging

deriving instance (Show a, Show mv) => Show (AStarState a mv)

-- --------------------
--
-- create initial state

initAStar :: a -> AStarState a mv         -- initial search state
initAStar b = AS S.empty q0 0 0 0.5       -- open states contain the start state
  where
    q0 = insertQ 0 (b, [], 0) emptyQ

-- the working horse

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

    cls'   = S.insert b cls               -- close current state
    news   = nextMoves b                  -- get next possible states

    opn'' = L.foldl' ins opn' news        -- insert new next states into open

    ins acc (mv', b')
      | b' `S.member` cls' = acc            -- ignore duplicates in open states
      | otherwise        = insertQ q' (b', mv' : p, c') acc
      where
        c' = c + moveCost  mv' b'           -- accumulate path costs
        h' =     estimatedCost b'
        q' = cw * c' + (1 - cw) * h'

-- --------------------
