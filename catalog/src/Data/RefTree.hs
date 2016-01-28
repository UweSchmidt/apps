{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.RefTree where

import           Data.Prim.Name
import           Data.Prim.ObjId
import           Data.Prim.Path
import           Data.Prim.TimeStamp

import           Control.Lens

-- import           Data.Monoid

import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as J


import           Data.Maybe

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- ----------------------------------------

-- A RefTree has a root reference and a map of refs to nodes

data RefTree node ref = RT !ref !(Map ref (node ref))

deriving instance (Show ref, Show (n ref)) => Show (RefTree n ref)

-- the constraint Ord b prevents a Functor instance

fmap' :: (Functor node, Ord ref') => (ref -> ref') -> RefTree node ref -> RefTree node ref'
fmap' f (RT r t) =
  RT (f r) ( M.foldrWithKey'
             (\ k v acc -> M.insert (f k) (fmap f v) acc)
             M.empty
             t
           )

rootRef :: Lens' (RefTree node ref) ref
rootRef k (RT r m) = fmap (\ new -> RT new m) (k r)

entries :: Lens' (RefTree node ref) (Map ref (node ref))
entries k (RT r m) = fmap (\ new -> RT r new) (k m)

-- ----------------------------------------

-- An UpLink adds two components to a node,
-- first a ref to the parent node,
-- second a name.
-- The root node has the root a parent ref

data UpLink  node ref = UL ref Name (node ref)

deriving instance (Show ref, Show (node ref)) => Show (UpLink  node ref)

instance Functor node => Functor (UpLink node) where
  fmap f (UL x n t) = UL (f x) n (fmap f t)

instance (ToJSON (node ref), ToJSON ref) => ToJSON (UpLink node ref) where
  toJSON (UL r n v) = J.object
    [ "parentRef" J..= r
    , "nodeName"  J..= n
    , "nodeVal"   J..= v
    ]

instance (FromJSON (node ref), FromJSON ref) => FromJSON (UpLink node ref) where
  parseJSON = J.withObject "UpLink" $ \ o ->
    UL
    <$> o J..: "parentRef"
    <*> o J..: "nodeName"
    <*> o J..: "nodeVal"

parentRef :: Lens' (UpLink node ref) ref
parentRef k (UL r n v) = fmap (\ new -> UL new n v) (k r)

nodeName :: Lens' (UpLink node ref) Name
nodeName k (UL r n v) = fmap (\ new -> UL r new v) (k n)

nodeVal :: Lens' (UpLink node ref) (node ref)
nodeVal k (UL r n v) = fmap (\ new -> UL r n new) (k v)

-- ----------------------------------------

checkJust :: String -> Iso' (Maybe a) a
checkJust msg = iso (fromMaybe (error msg)) Just

atRef :: (Functor f, Show (Index m), At m) =>
         Index m -> (IxValue m -> f (IxValue m)) -> m -> f m
atRef r = at r . checkJust ("atRef: undefined ref " ++ show r)

-- ----------------------------------------

type DirTree node ref = RefTree (UpLink node) ref

getParent :: (Ord ref, Show ref) => ref -> DirTree node ref -> ref
getParent r t
  = t ^. entries
       . atRef r
       . parentRef

getName :: (Ord ref, Show ref) => ref -> DirTree node ref -> Name
getName r t
  = t ^. entries
       . atRef r
       . nodeName


refPath :: (Ord ref, Show ref) => ref -> DirTree node ref -> Path
refPath r0 t
  = path (getParent r0 t) r0 (mkPath $ getName r0 t)
  where
    path pr r ps
      | pr == r   = ps
      | otherwise = path (getParent pr t) pr (consPath (getName r t)  ps)

-- ----------------------------------------


deriving instance (Show ref) => Show (DirNode ref)

instance Functor DirNode where
  fmap _ (F ts) = F ts
  fmap f (D m)  = D $ M.map f m


{-
isD :: Prism' (DirNode a) (DirNode a)
isD = is (\ d -> case d of
             D _ -> True
             _   -> False
         )

d' k (D m) = fmap (\ new -> D new) (k m)

ins' :: Ord a => (a -> n a -> n a) -> a -> a -> n a -> DirTree n a -> DirTree n a
ins' addChild' p r n rt =
  rt & entr' . at r .~ Just (UL p n)                -- add the child
     & entr' . at p . _Just . node' %~ addChild' r  -- insert in parent


addentr' :: Name -> a -> DirNode a -> DirNode a
addentr' nm r = isD . d' . at nm .~ Just r

-- addentr' nm r (D m) = D $ M.insert nm r m
-- addentr' _ _ f = f

insFS' :: Ord a => Name -> a -> a -> DirNode a -> DirTree DirNode a -> DirTree DirNode a
insFS' nm = ins' (addentr' nm)

u1 :: FSs
u1 = undefined
-- -}
-- ----------------------------------------
data DirNode a = F TimeStamp
               | D (Map Name a)

type FSwithNames = DirTree DirNode Name
type FSs = DirTree DirNode String

type FSwithIds = DirTree DirNode ObjId
