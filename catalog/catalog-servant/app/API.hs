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

import Network.HTTP.Media ((//), (/:))
import Servant

import Data.Prim
import Data.ImgTree (ImgNodeP)
import Data.MetaData
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
    JsonAPI
    :<|>
    NewDocAPI

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
-- and server version

type RootAPI
  = Capture "root" (BaseName HTMLStatic) :> Get '[HTMLStatic] LazyByteString
    :<|>
    "favicon.ico" :> Get '[ICO] LazyByteString
    :<|>
    "rpc.js" :> Get '[JSStatic] LazyByteString

-- ----------------------------------------
--
-- new URL API

type NewDocAPI
  = "docs" :>
    ( IconAPI
      :<|>
      IconpAPI
      :<|>
      ImgAPI
      :<|>
      PageAPI
    )

-- a lazy bytestring as response  with a cache control header

type CachedByteString
  = Headers '[Header "Cache-Control" Text] LazyByteString

--
-- icons: /icon/<w>x<h>/collections/<path>.jpg           -- collection icon
--        /icon/<w>x<h>/collections/<path>/pic-<ix>.jpg  -- col entry  icon
--
-- the .jpg extension and the pic-<ix> part must be parsed by the handler
-- due to restricted servant URL parsing capabilities

type IconAPI
  = "icon" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type IconpAPI
  = "iconp" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type ImgAPI
  = "img" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Header "Referer" Text :>
    Get '[JPEG] CachedByteString

type PageAPI
  = "page" :> Capture "geo" Geo':> CaptureAll "path" Text :>
    Get '[HTMLStatic] LazyByteString

-- ----------------------------------------
--
-- presentation API: .html album pages and .jpg images

-- generate blaze-1920x1200 and other geometry HTML pages

newtype Geo'      = Geo'      Geo

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
  = "get" :>
    ( "collection"    :> SimplePost ImgNodeP
      :<|>
      "isWriteable"   :> SimplePost Bool
      :<|>
      "isRemovable"   :> SimplePost Bool
      :<|>
      "isSortable"    :> SimplePost Bool
      :<|>
      "isCollection"  :> SimplePost Bool
      :<|>
      "blogcontents"  :> ParamPost Int Text
      :<|>
      "blogsource"    :> ParamPost Int Text
      :<|>
      "metadata"      :> ParamPost Int MetaData
      :<|>
      "rating"        :> ParamPost Int Rating
      :<|>
      "ratings"       :> SimplePost [Rating]
    )

-- the modifying ops

type JsonModifyAPI
  = "modify" :>
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

-- | Default parsing error.
defaultParseError :: Text -> Either Text a
defaultParseError input = Left ("could not parse: `" <> input <> "'")

-- ----------------------------------------
--
-- the static handlers are a bit clumsy
-- but no idea, how to deliver static files with right mimetype
-- with servant, no good example found yet

-- ----------------------------------------
--
-- JPEG static handler

data JPEG

instance Accept JPEG where
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG LazyByteString where
  mimeRender _ = id

instance HasExt JPEG where
  theExt _ = ".jpg"

-- ----------------------------------------
--
-- ICO static handler

data ICO

instance Accept ICO where
  contentType _ = "image" // "x-icon"

instance MimeRender ICO LazyByteString where
  mimeRender _ = id

instance HasExt ICO where
  theExt _ = ".ico"

-- ----------------------------------------
--
-- JSON static handler

data JSStatic

instance Accept JSStatic where
  contentType _ = "application" // "json"

instance MimeRender JSStatic LazyByteString where
  mimeRender _ = id

instance HasExt JSStatic where
  theExt _ = ".js"

-- --------------------
--
-- HTML static handler

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
