{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Prelude ()
import Prelude.Compat

-- import Control.Monad.Except
-- import Control.Monad.Reader
-- import Data.Aeson.Compat
-- import Data.Aeson.Types
-- import Data.Attoparsec.ByteString
-- import Data.ByteString (ByteString)
-- import Data.List
-- import Data.Maybe
-- import Data.String.Conversions
-- import Data.Time.Calendar
-- import GHC.Generics
-- import Lucid
-- import Network.HTTP.Media ((//), (/:))
-- import Network.Wai
-- import Network.Wai.Handler.Warp
import Servant
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
-- import qualified Data.Aeson.Parser
-- import qualified Text.Blaze.Html

-- import System.FilePath (FilePath)
import Data.Prim
import Web.HttpApiData (parseUrlPieceWithPrefix)
-- import qualified Data.Text as T


-- ----------------------------------------
-- the complete API

type CatalogAPI
  = ( BootstrapAPI
      :<|>
      AssetsAPI
      :<|>
      EditAPI
    )
    :<|>
    ( BlazeAPI
      :<|>
      ImgCopyAPI
    )
    :<|>
    JsonAPI

-- ----------------------------------------
--
-- static files API

-- static bootstrap files
type BootstrapAPI
  = "bootstrap" :> Raw

-- static asset files (css, icons, javascript)
type AssetsAPI
  = "assets" :>
    ( "css"        :> Raw
      :<|>
      "icons"      :> Raw
      :<|>
      "javascript" :> Raw
    )

-- main page for catalog edit
type EditAPI
  = "edit.html" :> Raw

-- ----------------------------------------
--
-- presentation API: .html album pages and .jpg images

-- generate blaze-1920x1200 and other geometry HTML pages

newtype BlazeHTML = BlazeHTML Geo'
newtype Geo'      = Geo'      Geo
newtype GeoAR'    = GeoAR'    GeoAR

type BlazeAPI
  = Capture    "blaze" BlazeHTML :>
    "archive"                    :>
    "collections"                :>
    CaptureAll "path"  Text      :> Get '[PlainText] String

-- generate image copies (fix-123x456, pad-123x456, ...)
type ImgCopyAPI
  = Capture    "geoar" GeoAR' :>
    ( "archive" :>
      CaptureAll "path"  Text  :> Get '[PlainText] String
      :<|>
      CaptureAll "path"  Text  :> Get '[PlainText] String
    )

-- ----------------------------------------
--
-- the JSON API for editing and viewing collection and images

type JsonAPI
  = JsonGetAPI
    :<|>
    JsonModifyAPI

-- simple op with a single path argument
-- and without request body
-- and a JSON result

type SimplePost r
  = CaptureAll "path" Text :> Post '[JSON] r

-- op with extra args given as request body
-- with JSON format

type ParamPost a r
  = CaptureAll "path" Text :> ReqBody '[JSON] a :> Post '[JSON] r

-- the query ops

type JsonGetAPI
  = "get-json" :>
    ( "isSortable"   :> SimplePost Bool
      :<|>
      "isCollection" :> SimplePost Bool
      :<|>
      "iconref"      :> ParamPost GeoAR (GeoAR, Path)
      :<|>
      "blogcontents" :> ParamPost Int (Int, Path)
      :<|>
      "blogsource"   :> ParamPost Int (Int, Path)
      :<|>
      "previewref"   :> ParamPost (Int, GeoAR) (Path, Int, GeoAR)
      :<|>
      "metadata"     :> ParamPost Int (Path, Int)
      :<|>
      "rating"       :> ParamPost Int (Path, Int)
      :<|>
      "ratings"      :> SimplePost Path
    )

-- the modifying ops

type JsonModifyAPI
  = "modify-json" :>
    ( "syncCol" :> SimplePost Path
      :<|>
      "saveblogsource" :> ParamPost (Int, Text) (Path, Int, Text)
    )

-- ----------------------------------------
--
-- Geo' and GeoAR' are helper to avoid
-- warnings about orphan instances for Geo and GeoAR

instance FromHttpApiData Geo' where
  parseUrlPiece s =
    maybe (defaultParseError s) Right $ g
    where
      g = Geo' <$> readGeo' (s ^. isoString)

instance FromHttpApiData GeoAR' where
  parseUrlPiece s =
    maybe (defaultParseError s) Right $ g
    where
      g = GeoAR' <$> readGeoAR (s ^. isoString)

instance FromHttpApiData BlazeHTML where
  parseUrlPiece input
    = BlazeHTML <$> parseUrlPieceWithPrefix "blaze-" input

-- | Default parsing error.
defaultParseError :: Text -> Either Text a
defaultParseError input = Left ("could not parse: `" <> input <> "'")

-- ----------------------------------------
