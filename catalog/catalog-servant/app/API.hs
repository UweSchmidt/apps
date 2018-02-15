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
-- import Data.ByteString.Lazy (ByteString)
-- import Data.List
-- import Data.Maybe
-- import Data.String.Conversions
-- import Data.Time.Calendar
-- import GHC.Generics
-- import Lucid
import Network.HTTP.Media ((//), (/:))
-- import Network.Wai
-- import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
-- import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html as Blaze

import System.FilePath (FilePath)
import Data.Prim
import Data.ImgTree (ImgNodeP)
import Data.MetaData
import Web.HttpApiData (parseUrlPieceWithPrefix)
import qualified Data.Text as T


-- ----------------------------------------
-- the complete API

type CatalogAPI
  = ( BootstrapAPI
      :<|>
      AssetsAPI
      :<|>
      RootAPI
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

-- root HTML pages (edit.html, index.html)
type RootAPI
  = Capture "root" (BaseName HTMLStatic) :> Get '[HTMLStatic] LazyByteString

-- ----------------------------------------
--
-- presentation API: .html album pages and .jpg images

-- generate blaze-1920x1200 and other geometry HTML pages

newtype BlazeHTML = BlazeHTML Geo'
newtype Geo'      = Geo'      Geo
newtype GeoAR'    = GeoAR'    GeoAR

-- gen page for paths like "/blaze-1920x1200/archive/collections/photos.html""
type BlazeAPI
  = Capture    "blaze" BlazeHTML :>
    CaptureAll "path"  Text      :> Get '[HTML] Blaze.Html

-- generate image copies (fix-123x456, pad-123x456, ...)
type ImgCopyAPI
  = Capture    "geoar" GeoAR' :>
    ( "archive" :>
      CaptureAll "path"  Text  :> Get '[JPEG] LazyByteString
      :<|>
      CaptureAll "path"  Text  :> Get '[JPEG] LazyByteString
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
    ( "collection"   :> SimplePost ImgNodeP
      :<|>
      "isWriteable"  :> SimplePost Bool
      :<|>
      "isRemovable"  :> SimplePost Bool
      :<|>
      "isSortable"   :> SimplePost Bool
      :<|>
      "isCollection" :> SimplePost Bool
      :<|>
      "iconref"      :> ParamPost GeoAR FilePath
      :<|>
      "blogcontents" :> ParamPost Int Text
      :<|>
      "blogsource"   :> ParamPost Int Text
      :<|>
      "previewref"   :> ParamPost (Int, GeoAR) FilePath
      :<|>
      "metadata"     :> ParamPost Int MetaData
      :<|>
      "rating"       :> ParamPost Int Rating
      :<|>
      "ratings"      :> SimplePost [Rating]
    )

-- the modifying ops

type JsonModifyAPI
  = "modify-json" :>
    ( "saveblogsource"       :> ParamPost (Int, Text) ()
      :<|>
      "changeWriteProtected" :> ParamPost ([Int], Bool) ()
      :<|>
      "sort"                 :> ParamPost [Int] ()
      :<|>
      "removeFromCollection" :> ParamPost [Int] ()
      :<|>
      "copyToCollection"     :> ParamPost ([Int], Path) ()
      :<|>
      "moveToCollection"     :> ParamPost ([Int], Path) ()
      :<|>
      "colimg"               :> ParamPost (Path, Int) ()
      :<|>
      "colblog"              :> ParamPost (Path, Int) ()
      :<|>
      "newcol"               :> ParamPost Name ()
      :<|>
      "renamecol"            :> ParamPost Name ()
      :<|>
      "setMetaData"          :> ParamPost ([Int], MetaData) ()
      :<|>
      "setMetaData1"         :> ParamPost (Int, MetaData) ()
      :<|>
      "setRating"            :> ParamPost ([Int], Rating) ()
      :<|>
      "setRating1"           :> ParamPost (Int, Rating) ()
      :<|>
      "snapshot"             :> ParamPost Text ()
      :<|>
      "syncCol"              :> SimplePost ()
      :<|>
      "syncExif"             :> SimplePost ()
      :<|>
      "newSubCols"           :> SimplePost ()
      :<|>
      "zipcollection"        :> SimplePost FilePath
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
--
-- JPEG handler

data JPEG

instance Accept JPEG where
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG LazyByteString where
  mimeRender _ = id

instance HasExt JPEG where
  theExt _ = ".jpg"

-- --------------------

data HTMLStatic

instance Accept HTMLStatic where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTMLStatic LazyByteString where
  mimeRender _ = id

instance HasExt HTMLStatic where
  theExt _ = ".html"


-- ----------------------------------------

class HasExt a where
  theExt :: Proxy a -> Text

  hasExt :: Proxy a -> Text -> Bool
  hasExt px t =
    T.length t > lx
    &&
    T.toLower (T.takeEnd lx t) == ex
    where
      ex = theExt px
      lx = T.length ex

-- ----------------------------------------

newtype BaseName a = BaseName {unBaseName :: Text}

instance HasExt a => FromHttpApiData (BaseName a) where
  parseUrlPiece s
    | hasExt px s = Right $ BaseName s
    | otherwise   = defaultParseError s
    where
      px :: Proxy a
      px = Proxy

-- ----------------------------------------
