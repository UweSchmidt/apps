{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Json
       ( jsonQuery
       , jsonModify
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

jsonQuery :: Text -> Text -> Cmd J.Value
jsonQuery fct path = do
  v <- lookupByPath path'
  case v of
    Nothing ->
      mkER $ fct <> ": entry not found: " <> path
    Just (i, n) ->
      case fct of
        -- get the object value
        "get-obj" -> do
          mkOK n

        -- get the iconref of a collection
        "get-iconref" -> do
          ref <- colImgRef i
          mkOK (ref ^. isoText)

        -- undefined get op
        _ ->
          mkER ("query operation not defined: " <> fct)
  where
    path' = path ^. isoString . from isoString

jsonModify :: Text -> Text -> Cmd J.Value
jsonModify fct _path =
  mkER ("modifying operation " <> fct <> " not defined")

-- ----------------------------------------
