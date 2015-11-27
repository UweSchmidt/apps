module Data.Map.Simple where

import           Data.Set.Simple
import qualified Data.Map  as M

-- ----------------------------------------

newtype Map k v = Map (M.Map k v)
                deriving (Eq, Ord)

instance (Ord k, Monoid v) => Monoid (Map k v) where
  mempty = emptyMap
  mappend = unionMap

emptyMap :: Map k v
emptyMap
  = Map M.empty

singletonMap :: k -> v -> Map k v
singletonMap k v
  = Map $ M.singleton k v

insertMap :: (Ord k, Monoid v) => k -> v -> Map k v -> Map k v
insertMap k v (Map m)
  = Map $ M.insertWith mappend k v m

unionMap :: (Ord k, Monoid v) => Map k v -> Map k v -> Map k v 
unionMap (Map m1) (Map m2)
  = Map $ M.unionWith mappend m1 m2

keys :: Ord k => Map k v -> Set k
keys (Map m)
  = fromList (M.keys m)

elems :: Map k v -> [v]
elems (Map m)
  = M.elems m
    
toListMap :: Map k a -> [(k, a)]
toListMap (Map m)
  = M.toList m

fromListMap :: Ord k => [(k, a)] -> Map k a
fromListMap
  = Map . M.fromList

lookupMap :: Ord k => k -> Map k a -> Maybe a
lookupMap k (Map m)
  = M.lookup k m

-- ----------------------------------------
