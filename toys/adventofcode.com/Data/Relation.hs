{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Relation where

import           Prelude hiding (foldr)
import qualified Prelude as P
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List (foldl')

-- ----------------------------------------

newtype Rel a b = Rel (Map a (Set b))

type Rel' a = Rel a a

-- ----------------------------------------

deriving instance (Eq a, Eq b) => Eq (Rel a b)
deriving instance (Show a, Show b) => Show (Rel a b)

-- ----------------------------------------

empty :: Rel a b
empty = Rel M.empty

null :: Rel a b -> Bool
null (Rel m) = M.null m

card, size :: Rel a b -> Int
size r = forEachS (\_ ys !acc -> (S.size ys + acc)) r 0
card   = size

member :: (Ord a, Ord b) => a -> b -> Rel a b -> Bool
member x y r =
  y `S.member` lookupS x r

lookupS :: (Ord a) => a -> Rel a b -> Set b
lookupS x (Rel m) = fromMaybe S.empty $ M.lookup x m

singleton :: a -> b -> Rel a b
singleton x y =
  Rel $ M.singleton x (S.singleton y)

singletonS :: (Ord a, Ord b) => a -> Set b -> Rel a b
singletonS x ys = insertS x ys empty

insert :: (Ord a, Ord b) => a -> b -> Rel a b -> Rel a b
insert x y = insertS x (S.singleton y)

insertS :: (Ord a, Ord b) => a -> Set b -> Rel a b -> Rel a b
insertS x ys r@(Rel m)
  | S.null ys = r
  | otherwise = Rel $ M.insertWith S.union x ys m

union :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
union (Rel m1) (Rel m2) =
  Rel $ M.unionWith S.union m1 m2

difference :: (Ord a, Ord b) => Rel a b -> Rel a b -> Rel a b
difference (Rel m1) (Rel m2) =
  Rel $ M.differenceWith diff m1 m2
  where
    diff s1 s2
      | S.null s' = Nothing
      | otherwise = Just s'
      where
        s' = s1 `S.difference` s2

{-# INLINE empty      #-}
{-# INLINE null       #-}
{-# INLINE member     #-}
{-# INLINE lookupS    #-}
{-# INLINE singleton  #-}
{-# INLINE singletonS #-}
{-# INLINE insert     #-}
{-# INLINE insertS    #-}
{-# INLINE union      #-}
{-# INLINE difference #-}

-- ----------------------------------------

dom :: Ord a => Rel a b -> Set a
dom (Rel m1) = S.fromList $ M.keys m1

rng :: Ord b => Rel a b -> Set b
rng r = forEachS (\_ ys acc -> ys `S.union` acc) r S.empty

toList :: Rel a b -> [(a, b)]
toList r = foldr (\x y -> ((x, y) :)) [] r

toListS :: Rel a b -> [(a, Set b)]
toListS r = foldrS (\x y -> ((x, y) :)) [] r

fromList :: (Ord a, Ord b) => [(a, b)] -> Rel a b
fromList = foldl' (flip (uncurry insert)) empty

{-# INLINE dom      #-}
{-# INLINE rng      #-}
{-# INLINE toList   #-}
{-# INLINE fromList #-}

-- ----------------------------------------
--
-- relational composition

comp :: (Ord a, Ord b, Ord c) => Rel a b -> Rel b c -> Rel a c
comp r1 r2 =
  forEach f1 r1 empty
  where
    f1 x y acc = insertS x (lookupS y r2) acc

filter :: (Ord a, Ord b) => (a -> b -> Bool) -> Rel a b -> Rel a b
filter p r =
  forEach f r empty
  where
    f x y
      | p x y     = insert x y
      | otherwise = id

filterS :: (Ord a, Ord b) => (a -> Set b -> Bool) -> Rel a b -> Rel a b
filterS p r =
  forEachS f r empty
  where
    f x ys
      | p x ys    = insertS x ys
      | otherwise = id

invert :: (Ord a, Ord b) => Rel a b -> Rel b a
invert r =
  forEach (flip insert) r empty

trClosure :: Ord a => Rel' a -> Rel' a
trClosure =
  fixpoint . iterate step
  where
    step r = r `union` (r `comp` r)

    fixpoint (x1 : xs@(x2 : _))
      | x1 == x2 = x1
      | otherwise = fixpoint xs

reflex :: Ord a => Rel' a -> Rel' a
reflex r = S.foldl' (\ acc x -> insert x x acc) empty $ dom r `S.union` rng r

{-# INLINE comp      #-}
{-# INLINE filter    #-}
{-# INLINE invert    #-}
{-# INLINE trClosure #-}
{-# INLINE reflex    #-}

-- ----------------------------------------
--
-- loop over all pairs (x, y) in rel

forEach :: (a -> b -> r -> r) -> Rel a b -> r -> r
forEach f = forEachS f1
  where
    f1 k = flip (S.foldl' (flip (f k)))

-- loop over all pair (x, ys) in the rel map

forEachS :: (a -> Set b -> r -> r) -> Rel a b -> r -> r
forEachS f (Rel m) =
  flip (M.foldlWithKey' (\ acc' k vs -> f k vs acc')) m

{-# INLINE forEach  #-}
{-# INLINE forEachS #-}

foldr :: (a -> b -> r -> r) -> r -> Rel a b -> r
foldr f = foldrS f1
  where
    f1 k vs acc = S.foldr (\ v acc' -> f k v acc') acc vs

foldrS :: (a -> Set b -> r -> r) -> r -> Rel a b -> r
foldrS f acc (Rel m) =
  M.foldrWithKey f acc m

{-# INLINE foldr  #-}
{-# INLINE foldrS #-}

-- ----------------------------------------
