module Data.Set.Simple.Partitions where

import Data.Map.Simple      (elems, singletonMap)
import Data.Set.Simple.Core

partitionBy :: (Ord a, Ord b) =>
               (a -> b) -> Set a -> Set (Set a)

partitionBy f xs
  = fromList . elems . foldMap (\ x -> singletonMap (f x) (singleton x)) $ xs

