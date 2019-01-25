{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Relation where

import           Prelude hiding (foldr, null, filter)
import qualified Prelude as P
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

apply :: (Ord a) => Rel a b -> a -> Set b
apply = flip lookupS

applyS :: (Ord a, Ord b) => Rel a b -> Set a -> Set b
applyS r xs = S.foldl' uni S.empty xs
  where
    uni res x = res `S.union` (r `apply` x)

findMin ::  (Ord a, Ord b) => Rel a b -> (a, b)
findMin r =
  (x, S.findMin s)
  where
    (x, s) = findMinS r

findMinS :: (Ord a) => Rel a b -> (a, Set b)
findMinS (Rel m)
  | M.null m  = error "findMinS: empty relation"
  | otherwise = M.findMin m

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

deleteFst :: (Ord a) => a -> Rel a b -> Rel a b
deleteFst x (Rel m) = Rel $ M.delete x m

deleteSnd :: (Ord a, Ord b) => b -> Rel a b -> Rel a b
deleteSnd y =
  foldrS f empty
  where
    f x ys res
      | not found        = insertS x ys  res
      | not (S.null ys') = insertS x ys' res
      | otherwise        =               res
      where
        found = y `S.member` ys
        ys'   = S.delete y ys

delete :: (Ord a) => a -> Rel' a -> Rel' a
delete x =
  deleteSnd x . deleteFst x

deleteS :: (Ord a) => Set a -> Rel' a -> Rel' a
deleteS xs r = S.fold delete r xs

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

differenceS :: (Ord a) => Rel a b -> Set a -> Rel a b
differenceS (Rel m1) s =
  Rel $ S.foldl' (flip M.delete) m1 s

{-# INLINE empty      #-}
{-# INLINE null       #-}
{-# INLINE member     #-}
{-# INLINE lookupS    #-}
{-# INLINE singleton  #-}
{-# INLINE singletonS #-}
{-# INLINE insert     #-}
{-# INLINE insertS    #-}
{-# INLINE delete     #-}
{-# INLINE deleteS    #-}
{-# INLINE union      #-}
{-# INLINE difference #-}
{-# INLINE differenceS #-}

unions :: (Ord a, Ord b) => [Rel a b] -> Rel a b
unions = foldl' union empty

-- ----------------------------------------

dom :: Ord a => Rel a b -> Set a
dom (Rel m1) = S.fromList $ M.keys m1

rng :: Ord b => Rel a b -> Set b
rng r = forEachS (\_ ys acc -> ys `S.union` acc) r S.empty

elems :: Ord a => Rel' a -> Set a
elems r = dom r `S.union` rng r

roots :: Ord a => Rel' a -> Set a
roots r =
  dom rcl `S.difference` rng rcl
  where
    rcl = trClosure r

leaves :: Ord a => Rel' a -> Set a
leaves r =
  rng rcl `S.difference` dom rcl
  where
    rcl = trClosure r

loops :: Ord a => Rel' a -> Set a
loops =
  foldrS f S.empty
  where
    f x ys res
      | x `S.member` ys = S.insert x res
      | otherwise       =            res

toList :: Rel a b -> [(a, b)]
toList r = foldr (\x y -> ((x, y) :)) [] r

toListS :: Rel a b -> [(a, Set b)]
toListS r = foldrS (\x y -> ((x, y) :)) [] r

fromList :: (Ord a, Ord b) => [(a, b)] -> Rel a b
fromList = foldl' (flip (uncurry insert)) empty

{-# INLINE dom      #-}
{-# INLINE rng      #-}
{-# INLINE elems    #-}
{-# INLINE roots    #-}
{-# INLINE leaves   #-}
{-# INLINE loops    #-}
{-# INLINE toList   #-}
{-# INLINE toListS  #-}
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

restrict :: Ord a => Set a -> Rel' a -> Rel' a
restrict s = filter inS
  where
    inS x y = x `S.member` s && y `S.member` s

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

symmetric :: Ord a => Rel' a -> Rel' a
symmetric r =
  r `union` invert r

{-# INLINE comp      #-}
{-# INLINE filter    #-}
{-# INLINE invert    #-}
{-# INLINE trClosure #-}
{-# INLINE reflex    #-}
{-# INLINE symmetric #-}

connectedComponents :: Ord a => Rel' a -> [Set a]
connectedComponents r0 = part [] r1
  where
    r1 = trClosure $ invert r0 `union` r0 `union` reflex r0

    part :: Ord a => [Set a] -> Rel' a -> [Set a]
    part s r
      | null r = s
      | otherwise = part (p : s) r'
      where
        (_x, p) = findMinS r
        r'      = differenceS r p

acyclic :: Ord a => Rel' a -> Bool
acyclic = null . filter (==) . trClosure

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
