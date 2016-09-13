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
    "collection" ->
      jl $ \ () -> do
      return n

    "iconref" ->
      jl $ \ () -> do
      (^. isoText) <$> colImgRef i

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
