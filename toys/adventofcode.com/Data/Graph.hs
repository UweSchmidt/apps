{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Graph where

import           Prelude hiding (foldr, null, filter)
import qualified Prelude as P
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List (foldl')
import           Data.Relation (Rel')
import qualified Data.Relation as R

-- ----------------------------------------

data Graph a = Gr { _nodes :: Set a
                  , _edges :: Rel' a
                  }

type Edge a  = (a, a)

-- ----------------------------------------

deriving instance (Eq a) => Eq (Graph a)
deriving instance (Show a) => Show (Graph a)

-- ----------------------------------------

empty :: Graph a
empty = Gr S.empty R.empty

null :: Graph a -> Bool
null = S.null . _nodes

size :: Graph a -> Int
size = S.size . _nodes

nodes :: Graph a -> Set a
nodes = _nodes

edges :: Graph a -> Rel' a
edges = _edges

dom :: Ord a => Graph a -> Set a
dom = R.dom . _edges

rng :: Ord a => Graph a -> Set a
rng = R.rng . _edges

roots :: Ord a => Graph a -> Set a
roots (Gr ns es) = ns `S.difference` R.rng es

leaves :: Ord a => Graph a -> Set a
leaves (Gr ns es) = ns `S.difference` R.dom es

loops :: Ord a => Graph a -> Set a
loops = R.loops . _edges

isolated :: Ord a => Graph a -> Set a
isolated g =
  (nodes g `S.difference` roots g) `S.difference` leaves g

member :: Ord a => a -> a -> Graph a -> Bool
member x y = R.member x y .  _edges

lookupS :: Ord a => a -> Graph a -> Set a
lookupS x = R.lookupS x . _edges

apply :: Ord a => Graph a -> a -> Set a
apply = flip lookupS

applyS :: Ord a => Graph a -> Set a -> Set a
applyS = R.applyS . _edges

findMin :: Ord a => Graph a -> Edge a
findMin = R.findMin . _edges

singleton :: Ord a => a -> a -> Graph a
singleton x y = insert x y empty

-- insert an edge
insert :: Ord a => a -> a -> Graph a -> Graph a
insert x y (Gr ns es) = Gr ns' es'
  where
    ns' = S.insert x . S.insert y $ ns
    es' = R.insert x y es

-- delete a single node x and all edges (x,y) and (y,x)
deleteN :: Ord a => a -> Graph a -> Graph a
deleteN x (Gr ns es) = Gr ns' es'
  where
    ns' = S.delete x ns
    es' = R.delete x es

-- delete a single edge (x,y)
deleteE :: Ord a => a -> a -> Graph a -> Graph a
deleteE x y (Gr ns es) = Gr ns es'
  where
    es' = R.difference es (R.singleton x y)

union :: Ord a => Graph a -> Graph a -> Graph a
union (Gr ns1 es1) (Gr ns2 es2) = Gr ns' es'
  where
    ns' = ns1 `S.union` ns2
    es' = es1 `R.union` es2

differenceR :: Ord a => Graph a -> Rel' a -> Graph a
differenceR (Gr ns1 es1) es2 = Gr ns1 es'
  where
    es' = es1 `R.difference` es2

toList :: Ord a => Graph a -> ([a], [Edge a])
toList (Gr ns es) = (S.toList ns, R.toList es)

fromList :: Ord a => [a] -> [Edge a] -> Graph a
fromList ns es =
  foldl' ins g0 es
  where
    g0             = Gr (S.fromList ns) R.empty
    ins res (x, y) = insert x y res

fromRel :: Ord a => Rel' a -> Graph a
fromRel r = Gr ns r
  where
    ns = R.dom r `S.union` R.rng r

-- ----------------------------------------

comp :: Ord a => Graph a -> Graph a -> Graph a
comp (Gr ns1 es1) (Gr ns2 es2) = Gr ns' es'
  where
    ns' = ns1 `S.union` ns2
    es' = es1 `R.comp`  es2

-- filter edges by a predicate
filter :: Ord a => (a -> a -> Bool) -> Graph a -> Graph a
filter p (Gr ns es) = Gr ns $ R.filter p es

-- remove all nodes not member of xs
-- and all edges containing nodes not member of xs
restrict :: Ord a => Set a -> Graph a -> Graph a
restrict xs (Gr ns es) = Gr ns' es'
  where
    ns' = ns `S.difference` xs
    es' = R.restrict xs es

invert :: Ord a => Graph a -> Graph a
invert (Gr ns es) = Gr ns $ R.invert es

trClosure :: Ord a => Graph a -> Graph a
trClosure (Gr ns es) = Gr ns $ R.trClosure es

-- add loops to all nodes
reflex :: Ord a => Graph a -> Graph a
reflex (Gr ns _es) = Gr ns es'
  where
    es' = S.fold ins R.empty ns
      where
        ins x res = R.insert x x res

acyclic :: Ord a => Graph a -> Bool
acyclic = R.acyclic . _edges

-- ----------------------------------------
