module Data.Set.Simple.Partitions where

import Data.Map.Simple      (elems, singletonMap)
import Data.Set.Simple.Core

-- | given a function f and a set s, compute the coasest partition, for which
-- all elements in every set of the resulting partition are mapped with f on the
-- same value in b
--
-- >>> partitionBy (`mod` 3) $ fromList [1..10]
-- {{1,4,7,10},{2,5,8},{3,6,9}}
--

partitionBy :: (Ord a, Ord b) =>
               (a -> b) -> Set a -> Set (Set a)

partitionBy f xs
  = fromList . elems . foldMap (\ x -> singletonMap (f x) (singleton x)) $ xs

-- | given a set of sets, compute the partition with the minimum # of sets,
-- such that every set in the argument can be represented as a union of members
-- of the resulting partition
--
-- >>> minPartition $ fromList $ map fromList $ [[1..10],[3..7],[2..8],[1..2]]
-- {{1},{2},{3,4,5,6,7},{8},{9,10}}
--

minPartition :: (Ord a) => Set (Set a) -> Set (Set a)
minPartition
  = foldr insertMinPart empty

-- given a set s0 and a partition, the partition is refined and extendet
-- such that the set s0 can be represented as union of 0, 1, 2 or 3 members of the partition

insertMinPart :: (Ord a) => Set a -> Set (Set a) -> Set (Set a)
insertMinPart s ps0
  | isEmpty s
      = ps0
  | otherwise
      = case minView ps0 of
         Nothing
           -> singleton s
         Just (s1, ps1)
           -> minPart s s1 ps1
  where
    addPart p1 ps
      | isEmpty p1 = ps
      | otherwise  = insert p1 ps
                     
    minPart s0 s1 ps1
      = addPart p0 . addPart p1 . insertMinPart s2 $ ps1
      where
        p0 = s0  `intersect`  s1  -- p0 and p1 are, if both not empty, a refinement of s1
        p1 = s1  `difference` p0
        s2 = s0  `difference` p0  -- the rest of s0 has to be inserted into the rest of the partition

-- t1 :: Set (Set Int)
-- t1 = minPartition $ fromList $ map fromList $ [[1..10],[3..7],[2..8],[1..2]]
