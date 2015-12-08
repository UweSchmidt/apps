{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Data.Set.Simple.Core where

import qualified Data.List as L
import qualified Data.Set  as S

-- ----------------------------------------

newtype Set a
  = Set (S.Set a)
  deriving (Foldable, Eq, Ord)

instance Ord a => Monoid (Set a) where
  mempty  = empty
  mappend = union

instance Show a => Show (Set a) where
  show (Set xs) = "{" ++ (L.intercalate "," $ map show $ S.toAscList xs) ++ "}"

-- --------------------

class FromSet f where
  fromSet :: Ord a => Set a -> f a

instance FromSet Set where
  fromSet = id

instance FromSet Maybe where
  fromSet = fmap fst . minView
                        
instance FromSet [] where
  fromSet = toList

-- --------------------
  
mkSet :: Ord a => [a] -> Set a
mkSet = Set . S.fromList

fromList :: Ord a => [a] -> Set a
fromList = mkSet

toList :: Set a -> [a]
toList (Set xs) = S.toAscList xs

empty :: Set a
empty = Set S.empty

singleton :: a -> Set a
singleton = Set . S.singleton

insert :: Ord a => a -> Set a -> Set a
insert x0 (Set xs) = Set $ S.insert x0 xs

isEmpty :: Set q -> Bool
isEmpty (Set xs) = S.null xs

member :: Ord a => a -> Set a -> Bool
member a (Set as) = a `S.member` as

isSubsetOf:: (Ord a) => Set a -> Set a -> Bool
isSubsetOf (Set s1) (Set s2) = s1 `S.isSubsetOf` s2

card :: Set a -> Int
card (Set as) = S.size as

union :: Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set $ s1 `S.union` s2 

unions :: Ord a => Set (Set a) -> Set a
unions = foldMap id

intersect :: Ord a => Set a -> Set a -> Set a
intersect (Set s1) (Set s2) = Set $ s1 `S.intersection` s2 

difference :: Ord a => Set a -> Set a -> Set a
difference (Set s1) (Set s2)
  = Set $ s1 `S.difference` s2
    
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint s1 s2
        = isEmpty $ s1 `intersect` s2

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs)
  = Set $ S.filter p xs

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
findMin (Set xs) = S.findMin xs

findMax :: Set a -> a
findMax (Set xs) = S.findMax xs

minView :: Ord a => Set a -> Maybe (a, Set a)
minView (Set xs) = (\ (y, ys) -> (y, Set ys)) <$> S.maxView xs

maxView :: Ord a => Set a -> Maybe (a, Set a)
maxView (Set xs) = (\ (y, ys) -> (y, Set ys)) <$> S.minView xs

-- ----------------------------------------
                           


