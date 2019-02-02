{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

-- an IntMap with Monoid instance

module Data.Monoid.IntMap
where

-- import           Data.Monoid()
import qualified Data.IntMap.Strict as M

newtype IntMap v = IM (M.IntMap v)
                   deriving
                     (Eq, Show, Functor, Foldable)

instance Semigroup v => Semigroup (IntMap v) where
  IM m1 <> IM m2 = IM $ M.unionWith (<>) m1 m2
  {-# INLINE (<>) #-}

instance Semigroup v => Monoid (IntMap v) where
  mempty = IM mempty
  {-# INLINE mempty #-}

singleton :: Int -> v -> IntMap v
singleton i v = IM $ M.singleton i v
{-# INLINE singleton #-}

fromList :: Monoid v => [(Int, v)] -> IntMap v
fromList = foldMap (uncurry singleton)
{-# INLINE fromList #-}

toList :: IntMap v -> [(Int, v)]
toList (IM m) = M.toList m
{-# INLINE toList #-}

lookup :: Int -> IntMap v -> Maybe v
lookup i (IM m) = M.lookup i m
{-# INLINE lookup #-}
