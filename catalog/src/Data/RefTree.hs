{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.RefTree where

import           Data.Prim.Name
import           Data.Prim.Path

import           Control.Lens
import           Control.Monad.Except
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

-- TODO: this isn't a solution, fmap does not fit
-- and the rename must be an Iso

renameRefTree :: (Functor node, Ord ref') => (ref -> ref') -> RefTree node ref -> RefTree node ref'
renameRefTree f (RT r t) =
  RT (f r) ( M.foldrWithKey'
             (\ k v acc -> M.insert (f k) (fmap f v) acc)
             M.empty
             t
           )

instance (ToJSON (node ref), ToJSON ref) => ToJSON (RefTree node ref) where
  toJSON (RT r m) = J.object
    [ "rootRef"   J..= r
    , "entries"   J..= M.toList m
    ]

instance (Ord ref, FromJSON (node ref), FromJSON ref) => FromJSON (RefTree node ref) where
  parseJSON = J.withObject "RefTree" $ \ o ->
    RT
    <$> o J..: "rootRef"
    <*> (M.fromList <$> o J..: "entries")

rootRef :: Lens' (RefTree node ref) ref
rootRef k (RT r m) = (\ new -> RT new m) <$> k r

entries :: Lens' (RefTree node ref) (Map ref (node ref))
entries k (RT r m) = (\ new -> RT r new) <$> k m

theNode :: (Ord ref, Show ref) =>
           ref -> Lens' (RefTree node ref) (node ref)
theNode r = entries . at r . checkJust ("atRef: undefined ref " ++ show r)

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

-- ----------------------------------------

type DirTree node ref = RefTree (UpLink node) ref

-- lenses

theParent :: (Ord ref, Show ref) => ref -> Lens' (DirTree node ref) ref
theParent r = theNode r . parentRef

theName ::  (Ord ref, Show ref) => ref -> Lens' (DirTree node ref) Name
theName r = theNode r . nodeName

theNodeVal ::  (Ord ref, Show ref) => ref -> Lens' (DirTree node ref) (node ref)
theNodeVal r = theNode r . nodeVal


-- access and modification

refPath :: (Ord ref, Show ref) => ref -> DirTree node ref -> Path
refPath r0 t
  = path (t ^. theParent r0) r0 (mkPath $ t ^. theName r0)
  where
    path pr r ps
      | pr == r   = ps
      | otherwise = path (t ^. theParent pr) pr (consPath (t ^. theName r)  ps)


-- | create the root of a DirTree.
-- It's the only node where the parent ref equals the ref.
-- The generation of the reference is supplied by a conversion from path to ref

mkDirRoot :: (Path -> ref) -> Name -> node ref -> DirTree node ref
mkDirRoot genRef n v
  = RT r (M.singleton r (UL r n v))
  where
    r = genRef $ mkPath n

isDirRoot :: (Ord ref, Show ref) => ref -> DirTree node ref -> Bool
isDirRoot r t = t ^. theParent r == r

-- | create a new entry with a new ref and modify the parent node
-- to store the new ref in its node value

mkDirNode :: (MonadError String m, Ord ref, Show ref) =>
             (Path -> ref) ->                      -- ref generator
             (node ref -> Bool) ->                 -- parent node editable?
             (ref -> node ref -> node ref) ->      -- edit value of parent node
             Name ->                               -- name of the node
             ref ->                                -- parent node
             node ref ->                           -- node value
             DirTree node ref ->                   -- tree
             m (ref, DirTree node ref) -- new ref and modified tree

mkDirNode genRef isParentDir updateParent n p v t
  = do when (has (entries . at r) t) $
         throwError $ "mkDirNode: entry already exists: " ++ show rp
       when (not (t ^. theNodeVal p . to isParentDir)) $
         throwError $ "mkDirNode: parent node not a dir" ++ show pp
       return
         ( r
         , t & entries . at r .~ Just (UL p n v)
             & theNodeVal p   %~ updateParent r
         )
  where
    pp = refPath p t
    rp = concPath pp (mkPath n)
    r  = genRef rp

remDirNode :: (MonadError String m, Ord ref, Show ref) =>
              (node ref -> Bool) ->
              (ref -> node ref -> node ref) ->
              ref ->
              DirTree node ref ->
              m (DirTree node ref)
remDirNode removable updateParent r t
  = do when (hasn't (entries . at r) t) $
         throwError $ "remDirNode: ref doesn't exist: " ++ show r

       when (r `isDirRoot` t) $
         throwError $ "remDirNode: root ref can't be removed"

       when (not (removable (t ^. theNode r . nodeVal))) $
         throwError $ "remDirNode: node value not removable, entry: "
                      ++ show (refPath r t)

       return (t & theNodeVal p   %~ updateParent r
                 & entries . at r .~ Nothing
              )
   where
     p = t ^. theParent r

-- ----------------------------------------
{-

deriving instance (Show ref) => Show (DirNode ref)

instance Functor DirNode where
  fmap _ (F ts) = F ts
  fmap f (D m)  = D $ M.map f m



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

-- ----------------------------------------
data DirNode a = F TimeStamp
               | D (Map Name a)

type FSwithNames = DirTree DirNode Name
type FSs = DirTree DirNode String

type FSwithIds = DirTree DirNode ObjId

-- -}
