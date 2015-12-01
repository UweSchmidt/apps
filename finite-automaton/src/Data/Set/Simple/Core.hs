{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Set.Simple.Core where

import qualified Data.List as L

-- ----------------------------------------

newtype Set a
  = Set [a]
  deriving (Functor, Foldable, Eq, Ord)

instance Ord a => Monoid (Set a) where
  mempty  = empty
  mappend = union

instance Show a => Show (Set a) where
  show (Set xs) = "{" ++ show xs ++ "}"
  
mkSet :: Ord a => [a] -> Set a
mkSet = Set . L.sort

fromList :: Ord a => [a] -> Set a
fromList = mkSet . L.nub

toList :: Set a -> [a]
toList (Set xs) = xs

empty :: Set a
empty = Set []

singleton :: a -> Set a
singleton x = Set [x]

insert :: Ord a => a -> Set a -> Set a
insert x0 (Set xs) = Set $ ins x0 xs
  where
    ins x [] = [x]
    ins x ys@(y : ys1)
      | x <  y    = x : ys
      | x == y    =     ys
      | otherwise = y : ins x ys1

isEmpty :: Set q -> Bool
isEmpty (Set xs) = null xs

member :: Ord a => a -> Set a -> Bool
member a (Set as) = a `elem` as

isSubsetOf:: (Ord a) => Set a -> Set a -> Bool
isSubsetOf s1 s2 = (s1 `intersect` s2) == s1

card :: Set a -> Int
card (Set as) = length as

union :: Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2) = mkSet $ s1 `L.union` s2 

unions :: Ord a => Set (Set a) -> Set a
unions = foldMap id

intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set s1) (Set s2) = mkSet $ s1 `L.intersect` s2 

difference :: Ord a => Set a -> Set a -> Set a
difference (Set s1) (Set s2)
  = Set $ s1 L.\\ s2
    
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint s1 s2
        = isEmpty $ s1 `intersect` s2

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs)
  = Set $ filter p xs

lookupSet :: Ord a => a -> Set (Set a) -> Set a
lookupSet e xss
  | isEmpty xss' = empty
  | otherwise    = findMin xss'
  where
    xss' = filterSet ( e `member`) xss

lookupSubset :: Ord a => Set a -> Set (Set a) -> Set a
lookupSubset xs xss
  | isEmpty xss' = empty
  | otherwise    = findMin xss'
  where
    xss' = filterSet (xs `isSubsetOf`) xss

findMin :: Set a -> a
findMin (Set xs) = head xs

findMax :: Set a -> a
findMax (Set xs) = last xs

-- ----------------------------------------
                           


