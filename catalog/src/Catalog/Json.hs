{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Json
       ( jsonRPC
       )
where

import           Catalog.Cmd
import           Catalog.Html.Photo2 (colImgRef)
-- import           Catalog.Journal
-- import           Catalog.Cmd.Types
import           Control.Lens
-- import           Control.Monad.Except
-- import           Control.Monad.RWSErrorIO
-- import           Data.Prim
import           Data.ImageStore
import           Data.ImgNode
import           Data.ImgTree
import           Data.Prim
import qualified Data.Aeson as J
-- import qualified Data.Aeson.Encode.Pretty as J

-- ----------------------------------------

data JsonRes a = OK a
               | ER Text

deriving instance (Show a) => Show (JsonRes a)

instance ToJSON a => ToJSON (JsonRes a) where
  toJSON (OK x) = J.object ["res" J..= x]
  toJSON (ER e) = J.object ["err" J..= e]

mkOK :: (ToJSON a) => a -> Cmd J.Value
mkOK x = return $ J.toJSON x

mkER :: Text -> Cmd J.Value
mkER t = return $ J.toJSON $ (ER t :: JsonRes ())

-- ----------------------------------------
-- AJAX JSON interface
--
-- all requests consists of a JSON array with 2 elements,
-- 1. the name of an RPC function
-- 2. an array of 2 arguments
--  2.1. an object id, represented as a path (string)
--  2.2. an arbitrary JSON value for extra data,
--       mostly needed in modifying function, e.g. an array of indexes for sorting

-- 1. step: parse the json function name

jsonRPC :: J.Value -> Cmd J.Value
jsonRPC jv = do
  case  J.fromJSON jv :: J.Result (Text, (Text, J.Value)) of
    J.Error e ->
      mkER $ "illegal JSON RPC call: " <> e ^. isoText
    J.Success (fct, (path, args)) -> do
      let path' = path ^. isoString . from isoString
      v <- lookupByPath path'
      case v of
        Nothing ->
          mkER $ fct <> ": entry not found: " <> path
        Just (i, n) ->
          jsonCall fct i n args

-- 3. step: dispatch over the function name
-- make a JSON call of cmd fct with an ObjId i,
-- an associated ImgNode n
-- and an extra JSON argument args

jsonCall :: Text -> ObjId -> ImgNode -> J.Value -> Cmd J.Value
jsonCall fct i n args =
  case fct of

    -- read a whole collection
    "collection" ->
      jl $ \ () -> do
      return n

    -- read the src path for a collection icon
    -- result is an url pointing to the icon src
    "iconref" ->
      jl $ \ () -> do
      (^. isoText) <$> colImgRef i

    -- sort a collection by sequence of positions
    -- result is the new collection
    "sort" -> do
      jl $ \ ixs -> do
        sortColByIxList ixs i
        getImgVal i

    -- set or unset the collection image
    -- i must reference a collection, not an image
    -- nothing is returned
    "colimg" -> do
      jl $ \ ix' -> do
        setColImg ix' i n

    -- unimplemented operations
    _ -> mkER $ "illegal JSON RPC function: " <> fct
  where
    jl :: (FromJSON a, ToJSON b) => (a -> Cmd b) -> Cmd J.Value
    jl = flip jsonLift args

-- 3., 4. and 5. step: parse the extra JSON argument
-- make the call of the internal operation and
-- convert the result back to JSON

jsonLift :: (FromJSON a, ToJSON b) => (a -> Cmd b) -> (J.Value -> Cmd J.Value)
jsonLift cmd jv =
  case J.fromJSON jv of
    J.Error e ->
      mkER $ "illegal JSON post arg: " <> e ^. isoText
    J.Success v ->
      cmd v >>= mkOK

-- ----------------------------------------

-- sort a collection by a list of positions
--
-- 1. all entries with "-1" mark and pos less than last marked entry
-- 2. last marked entry
-- 3. all other marked entries ordered by mark count
-- 4. all entries with "-1" mark and pos greater than largest mark index

sortColByIxList :: [Int] -> ObjId -> Cmd ()
sortColByIxList ixs oid
  | null ixs =
      return ()
  | otherwise =
    adjustColEntries (reorderCol ixs) oid

reorderCol :: [Int] -> [a] -> [a]
reorderCol ixs cs =
  map snd . sortBy (cmp mx `on` fst) $ zip ixs' cs
  where
    ixs' :: [(Int, Int)]
    ixs' = zip ixs [0..]

    mx :: (Int, Int)
    mx = maximum ixs'

cmp :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
cmp (mi, mx) (i, x) (j, y)
      | i == -1 && j == -1 =
        compare x y
      | i == -1 && j >= 0 =
        compare x mx
      | i >= 0  && j == -1 =
        compare mx y
      | i == mi && j >= 0 =
        LT
      | i >= 0  && j == mi =
        GT
      | i >= 0  && j >= 0 =
        compare i j
      | otherwise =
        EQ

-- ----------------------------------------
--
-- set or unset the "front page" image of a collection
-- to one of the images in the collection
-- The pos param specifies the position or, if -1, the unset op

setColImg :: Int -> ObjId -> ImgNode -> Cmd ()
setColImg pos oid n
  | Just (iid, inm, _im) <- n ^? theColEntries . ix pos . theColImgRef =
      adjustColImg (const $ Just (iid, inm)) oid

  | otherwise =
      adjustColImg (const Nothing) oid

-- ----------------------------------------
