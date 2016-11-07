{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Catalog where

import Data.Prim
import qualified Data.IntMap     as IM
import qualified Data.Map.Strict as M


class CatalogKey k where
  data CatMap k :: * -> *
  emptyCat    :: CatMap k v
  lookupCat   :: k -> CatMap k v -> Maybe v
  insertCat   :: k -> v -> CatMap k v -> CatMap k v

instance CatalogKey Int where
  newtype CatMap Int v        = CatMapInt (IM.IntMap v)
  emptyCat                    = CatMapInt IM.empty
  lookupCat k   (CatMapInt m) = IM.lookup k m
  insertCat k v (CatMapInt m) = CatMapInt (IM.insert k v m)

instance CatalogKey ObjId where
  newtype CatMap ObjId v        = CatMapObjId (IM.IntMap v)
  emptyCat                      = CatMapObjId IM.empty
  lookupCat k   (CatMapObjId m) = IM.lookup (k ^. objId2Int) m
  insertCat k v (CatMapObjId m) = CatMapObjId (IM.insert (k ^. objId2Int) v m)

instance CatalogKey Path where
  newtype CatMap Path v         = CatMapPath (M.Map Path v)
  emptyCat                      = CatMapPath M.empty
  lookupCat k   (CatMapPath m)  = M.lookup k m
  insertCat k v (CatMapPath m)  = CatMapPath (M.insert k v m)
