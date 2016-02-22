{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
where

import           Data.Prim.Name
import           Data.Prim.Path
import qualified Data.Vector as V
import           Control.Lens
import           Control.Monad.Except

import           Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as J


import           Data.Maybe

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- ----------------------------------------

newtype MetaData = MD (Map Name Name)

-- ----------------------------------------

deriving instance Show MetaData

instance ToJSON MetaData where
  toJSON (MD m) = J.Array $ V.singleton $
    J.object $ map (uncurry toPair) $ M.toList m
    where
      toPair n v =
        (n ^. name2string . to T.pack) J..= v

{-}
instance (Ord ref, FromJSON (node ref), FromJSON ref) => FromJSON (RefTree node ref) where
  parseJSON = J.withObject "RefTree" $ \ o ->
    RT
    <$> o J..: "rootRef"
    <*> (M.fromList <$> o J..: "entries")

rootRef :: Lens' (RefTree node ref) ref
rootRef k (RT r m) = (\ new -> RT new m) <$> k r

entries :: Lens' (RefTree node ref) (Map ref (node ref))
entries k (RT r m) = (\ new -> RT r new) <$> k m

entryAt :: (Ord ref) => ref -> Lens' (RefTree node ref) (Maybe (node ref))
entryAt r = entries . at r

theNode :: (Ord ref, Show ref) =>
           ref -> Lens' (RefTree node ref) (node ref)
theNode r = entryAt r . checkJust ("atRef: undefined ref " ++ show r)
-- -}
-- ----------------------------------------
