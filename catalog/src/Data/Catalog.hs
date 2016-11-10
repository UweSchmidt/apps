{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Catalog where

import           Data.Prim
import qualified Data.IntMap     as IM
import qualified Data.Aeson      as J
import qualified Data.Map.Strict as M


class CatalogKey k where
  data CatMap k :: * -> *
  emptyCat    :: CatMap k v
  lookupCat   :: k -> CatMap k v -> Maybe v
  insertCat   :: k -> v -> CatMap k v -> CatMap k v
  toListCat   :: CatMap k v -> [(k,v)]

-- just a test impl
instance CatalogKey Int where
  newtype CatMap Int v        = CatMapInt (IM.IntMap v)
  emptyCat                    = CatMapInt IM.empty
  lookupCat k   (CatMapInt m) = IM.lookup k m
  insertCat k v (CatMapInt m) = CatMapInt (IM.insert k v m)
  toListCat     (CatMapInt m) = IM.toList m

-- catalog map as IntMap
instance CatalogKey ObjId where
  newtype CatMap ObjId v        = CatMapObjId (IM.IntMap v)
  emptyCat                      = CatMapObjId IM.empty
  lookupCat k   (CatMapObjId m) = IM.lookup (k ^. objId2Int) m
  insertCat k v (CatMapObjId m) = CatMapObjId (IM.insert (k ^. objId2Int) v m)
  toListCat     (CatMapObjId m) = map (first (^. from objId2Int)) $ IM.toList m

-- catalog map as Map with Path as key
instance CatalogKey Path where
  newtype CatMap Path v         = CatMapPath (M.Map Path v)
  emptyCat                      = CatMapPath M.empty
  lookupCat k   (CatMapPath m)  = M.lookup k m
  insertCat k v (CatMapPath m)  = CatMapPath (M.insert k v m)
  toListCat     (CatMapPath m)  = M.toList m

instance (ToJSON v) => ToJSON (CatMap ObjId v) where
  toJSON m = toJSON $ toListCat m

instance (ToJSON v) => ToJSON (CatMap Path v) where
  toJSON m = toJSON $ toListCat m
