{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Catalog where

import           Data.Prim
import qualified Data.IntMap     as IM
import qualified Data.Aeson      as J
import qualified Data.Map.Strict as M


class CatalogKey k where
  data CatMap k :: * -> *
  emptyCat    :: CatMap k v
  singletonCat:: k -> v -> CatMap k v
  lookupCat   :: k -> CatMap k v -> Maybe v
  insertCat   :: k -> v -> CatMap k v -> CatMap k v
  deleteCat   :: k ->      CatMap k v -> CatMap k v
  foldCat     :: (k -> v -> r -> r) -> r -> CatMap k v -> r
  toListCat   :: CatMap k v -> [(k,v)]
  fromListCat :: [(k,v)] -> CatMap k v

  singletonCat k v = insertCat k v emptyCat

-- just a test impl
instance CatalogKey Int where
  newtype CatMap Int v          = CatMapInt (IM.IntMap v)
  emptyCat                      = CatMapInt IM.empty
  lookupCat k   (CatMapInt m)   = IM.lookup k m
  insertCat k v (CatMapInt m)   = CatMapInt (IM.insert k v m)
  deleteCat k   (CatMapInt m)   = CatMapInt (IM.delete k   m)
  foldCat f acc (CatMapInt m)   = IM.foldWithKey f acc m
  toListCat     (CatMapInt m)   = IM.toList m
  fromListCat                   = CatMapInt . IM.fromList

-- catalog map as IntMap
instance CatalogKey ObjId where
  newtype CatMap ObjId v        = CatMapObjId (IM.IntMap v)
  emptyCat                      = CatMapObjId IM.empty
  lookupCat k   (CatMapObjId m) = IM.lookup (k ^. objId2Int) m
  insertCat k v (CatMapObjId m) = CatMapObjId (IM.insert (k ^. objId2Int) v m)
  deleteCat k   (CatMapObjId m) = CatMapObjId (IM.delete (k ^. objId2Int)   m)
  foldCat f acc (CatMapObjId m) = IM.foldWithKey f' acc m
                                  where
                                    f' k = f (k ^. from objId2Int)
  toListCat     (CatMapObjId m) = map (first (^. from objId2Int)) $ IM.toList m
  fromListCat                   = CatMapObjId . IM.fromList . map (first (^. objId2Int))

-- catalog map as Map with Path as key
instance CatalogKey Path where
  newtype CatMap Path v         = CatMapPath (M.Map Path v)
  emptyCat                      = CatMapPath M.empty
  lookupCat k   (CatMapPath m)  = M.lookup k m
  insertCat k v (CatMapPath m)  = CatMapPath (M.insert k v m)
  deleteCat k   (CatMapPath m)  = CatMapPath (M.delete k   m)
  foldCat f acc (CatMapPath m)  = M.foldrWithKey f acc m
  toListCat     (CatMapPath m)  = M.toList m
  fromListCat                   = CatMapPath . M.fromList

instance (ToJSON v) => ToJSON (CatMap ObjId v) where
  toJSON m = toJSON $ toListCat m

instance (ToJSON v) => ToJSON (CatMap Path v) where
  toJSON m = toJSON $ toListCat m

instance (FromJSON v) => FromJSON (CatMap ObjId v) where
  parseJSON l = fromListCat <$> parseJSON l

instance (FromJSON v) => FromJSON (CatMap Path v) where
  parseJSON l = fromListCat <$> parseJSON l

at' :: CatalogKey ref => ref -> Lens' (CatMap ref v) (Maybe v)
at' k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (deleteCat k m)) mv
    Just v' -> insertCat k v' m
    where mv = lookupCat k m
{-# INLINE at' #-}

-- ----------------------------------------

data RefTree' cat node ref = RT' !ref !(cat ref (node ref))

instance (ToJSON ref, ToJSON (cat ref (node ref))) =>
         ToJSON (RefTree' cat node ref) where
  toJSON (RT' r m) = J.object
    [ "rootRef"   J..= r
    , "entries"   J..= m
    ]

type RefTreeObjId node = RefTree' CatMap node ObjId
type RefTreePath  node = RefTree' CatMap node Path

rootRef' :: Lens' (RefTree' cat node ref) ref
rootRef' k (RT' r m) = (\ new -> RT' new m) <$> k r
{-# INLINE rootRef' #-}

entries' :: Lens' (RefTree' cat node ref) (cat ref (node ref))
entries' k (RT' r m) = (\ new -> RT' r new) <$> k m
{-# INLINE entries' #-}

entryAt' :: (CatalogKey ref) => ref -> Lens' (RefTree' CatMap node ref) (Maybe (node ref))
entryAt' r = entries' . at' r
{-# INLINE entryAt' #-}

theNode' :: (CatalogKey ref, Show ref) =>
            ref -> Lens' (RefTree' CatMap node ref) (node ref)
theNode' r = entryAt' r . checkJust' ("atRef: undefined ref " ++ show r)
{-# INLINE theNode' #-}

-- ----------------------------------------

mapRefTree' :: (Functor node, CatalogKey ref, CatalogKey ref') =>
               (ref -> ref') -> RefTree' CatMap node ref -> RefTree' CatMap node ref'
mapRefTree' f (RT' r t) =
  RT' (f r) (foldCat f' emptyCat t)
  where
    f' k v (!acc) = insertCat (f k) (f <$> v) acc

-- ----------------------------------------

checkJust' :: String -> Iso' (Maybe a) a
checkJust' msg = iso (fromMaybe (error msg)) Just
{-# INLINE checkJust' #-}

-- ----------------------------------------

type DirTree' cat node ref = RefTree' cat (UpLink' node) ref

-- lenses

theParent' :: (CatalogKey ref, Show ref) => ref -> Lens' (DirTree' CatMap node ref) ref
theParent' r = theNode' r . parentRef'
{-# INLINE theParent' #-}

theName' ::  (CatalogKey ref, Show ref) => ref -> Lens' (DirTree' CatMap node ref) Name
theName' r = theNode' r . nodeName'
{-# INLINE theName' #-}

theNodeVal' :: (CatalogKey ref, Show ref) => ref -> Lens' (DirTree' CatMap node ref) (node ref)
theNodeVal' r = theNode' r . nodeVal'
{-# INLINE theNodeVal' #-}

-- ----------------------------------------

data UpLink'  node ref = UL' !ref !Name !(node ref)

deriving instance (Show ref, Show (node ref)) => Show (UpLink'  node ref)

instance Functor node => Functor (UpLink' node) where
  fmap f (UL' x n t) = UL' (f x) n (fmap f t)

instance (ToJSON (node ref), ToJSON ref) => ToJSON (UpLink' node ref) where
  toJSON (UL' r n v) = J.object
    [ "parentRef" J..= r
    , "nodeName"  J..= n
    , "nodeVal"   J..= v
    ]

instance (FromJSON (node ref), FromJSON ref) => FromJSON (UpLink' node ref) where
  parseJSON = J.withObject "UpLink" $ \ o ->
    UL'
    <$> o J..: "parentRef"
    <*> o J..: "nodeName"
    <*> o J..: "nodeVal"

parentRef' :: Lens' (UpLink' node ref) ref
parentRef' k (UL' r n v) = fmap (\ new -> UL' new n v) (k r)
{-# INLINE parentRef' #-}

nodeName' :: Lens' (UpLink' node ref) Name
nodeName' k (UL' r n v) = fmap (\ new -> UL' r new v) (k n)
{-# INLINE nodeName' #-}

nodeVal' :: Lens' (UpLink' node ref) (node ref)
nodeVal' k (UL' r n v) = fmap (\ new -> UL' r n new) (k v)
{-# INLINE nodeVal' #-}

-- ----------------------------------------

-- access and modification

refPath' :: (CatalogKey ref, Eq ref, Show ref) => ref -> DirTree' CatMap node ref -> Path
refPath' r0 t
  = path r0 (mkPath $ t ^. theName' r0)
  where
    path ref acc
      | isRoot    = acc
      | otherwise = path par (consPath (t ^. theName' par) acc)
      where
        par    = t ^. theParent' ref
        isRoot = par == ref
{-# INLINE refPath' #-}

mkDirRoot' :: (CatalogKey ref) =>
              (Path -> ref) -> Name -> node ref -> DirTree' CatMap node ref
mkDirRoot' genRef n v
  = RT' r (singletonCat r (UL' r n v))
  where
    r = genRef $ mkPath n

isDirRoot' :: (CatalogKey ref, Eq ref, Show ref) => ref -> DirTree' CatMap node ref -> Bool
isDirRoot' r t = t ^. theParent' r == r
{-# INLINE isDirRoot' #-}

lookupDirPath' :: (CatalogKey ref, Show ref) =>
                  (Path -> ref) ->
                  Path ->
                  DirTree' CatMap node ref ->
                  Maybe (ref, node ref)
lookupDirPath' genRef p t =
   (\ v -> (i', v)) <$> (t ^? entryAt' i' . _Just . nodeVal')
  where
    i' = genRef p
{-# INLINE lookupDirPath' #-}

-- ----------------------------------------
