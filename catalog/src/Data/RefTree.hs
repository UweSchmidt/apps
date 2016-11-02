{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.RefTree
       ( RefTree
       , UpLink
       , DirTree
       , rootRef
       , entries
       , entryAt
       , theNode
       , parentRef
       , nodeName
       , nodeVal
       , theParent
       , theName
       , theNodeVal
       , refPath
       , mkDirRoot
       , isDirRoot
       , mkDirNode
       , remDirNode
       , lookupDirPath
       , mapRefTree
       )
where

import           Control.Monad.Except
import           Data.Prim
import qualified Data.Aeson as J
import qualified Data.Map.Strict as M

-- ----------------------------------------

-- A RefTree has a root reference and a map of refs to nodes

data RefTree node ref = RT !ref !(Map ref (node ref))

deriving instance (Show ref, Show (n ref)) => Show (RefTree n ref)

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
{-# INLINE rootRef #-}

entries :: Lens' (RefTree node ref) (Map ref (node ref))
entries k (RT r m) = (\ new -> RT r new) <$> k m
{-# INLINE entries #-}

entryAt :: (Ord ref) => ref -> Lens' (RefTree node ref) (Maybe (node ref))
entryAt r = entries . at r
{-# INLINE entryAt #-}

theNode :: (Ord ref, Show ref) =>
           ref -> Lens' (RefTree node ref) (node ref)
theNode r = entryAt r . checkJust ("atRef: undefined ref " ++ show r)
{-# INLINE theNode #-}

-- ----------------------------------------

-- exchange the keys of a reftree
--
-- almost a functor, f must be an injective function

mapRefTree :: (Functor node, Ord ref') =>
              (ref -> ref') ->
              RefTree node ref -> RefTree node ref'
mapRefTree f (RT r t) =
  RT (f r) ( M.foldrWithKey'
             (\ k !v !acc -> M.insert (f k) (fmap f v) acc)
             M.empty
             t
           )

-- ----------------------------------------

-- An UpLink adds two components to a node,
-- first a ref to the parent node,
-- second a name.
-- The root node has the root and a parent ref

data UpLink  node ref = UL !ref !Name !(node ref)

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
{-# INLINE parentRef #-}

nodeName :: Lens' (UpLink node ref) Name
nodeName k (UL r n v) = fmap (\ new -> UL r new v) (k n)
{-# INLINE nodeName #-}

nodeVal :: Lens' (UpLink node ref) (node ref)
nodeVal k (UL r n v) = fmap (\ new -> UL r n new) (k v)
{-# INLINE nodeVal #-}

-- ----------------------------------------

checkJust :: String -> Iso' (Maybe a) a
checkJust msg = iso (fromMaybe (error msg)) Just
{-# INLINE checkJust #-}

-- ----------------------------------------

type DirTree node ref = RefTree (UpLink node) ref

-- lenses

theParent :: (Ord ref, Show ref) => ref -> Lens' (DirTree node ref) ref
theParent r = theNode r . parentRef
{-# INLINE theParent #-}

theName ::  (Ord ref, Show ref) => ref -> Lens' (DirTree node ref) Name
theName r = theNode r . nodeName
{-# INLINE theName #-}

theNodeVal ::  (Ord ref, Show ref) => ref -> Lens' (DirTree node ref) (node ref)
theNodeVal r = theNode r . nodeVal
{-# INLINE theNodeVal #-}

-- ----------------------------------------

-- access and modification

refPath :: (Ord ref, Show ref) => ref -> DirTree node ref -> Path
refPath r0 t
  = path r0 (mkPath $ t ^. theName r0)
  where
    path ref acc
      | isRoot    = acc
      | otherwise = path par (consPath (t ^. theName par) acc)
      where
        par    = t ^. theParent ref
        isRoot = par == ref
{-# INLINE refPath #-}

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
{-# INLINE isDirRoot #-}

-- lookup a (ref, nodeval) by a path
lookupDirPath :: (Ord ref, Show ref) =>
                  (Path -> ref) ->
                  Path ->
                  DirTree node ref ->
                  Maybe (ref, node ref)
lookupDirPath genRef p t =
   (\ v -> (i', v)) <$> (t ^? entryAt i' . _Just . nodeVal)
  where
    i' = genRef p
{-# INLINE lookupDirPath #-}



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
  = do when (has (entryAt r . _Just) t) $
         throwError $ "mkDirNode: entry already exists: " ++ show rp
       unless (t ^. theNodeVal p . to isParentDir) $
         throwError $ "mkDirNode: parent node not a dir: " ++ show pp
       return
         ( r
         , t & entryAt r    .~ Just (UL p n v)
             & theNodeVal p %~ updateParent r
         )
  where
    pp = refPath p t
    rp = pp `snocPath` n
    r  = genRef rp

remDirNode :: (MonadError String m, Ord ref, Show ref) =>
              (node ref -> Bool) ->
              (ref -> node ref -> node ref) ->
              ref ->
              DirTree node ref ->
              m (DirTree node ref)
remDirNode removable updateParent r t
  = do unless (has (entryAt r) t) $
         throwError $ "remDirNode: ref doesn't exist: " ++ show r

       when (r `isDirRoot` t) $
         throwError $ "remDirNode: root ref can't be removed"

       unless (removable (t ^. theNode r . nodeVal)) $
         throwError $ "remDirNode: node value not removable, entry: "
                      ++ show (refPath r t)

       return (t & theNodeVal p %~ updateParent r
                 & entryAt r    .~ Nothing
              )
   where
     p = t ^. theParent r

-- ----------------------------------------
