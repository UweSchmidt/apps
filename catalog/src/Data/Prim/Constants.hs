{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Constants where

import Data.Prim.Name
import Data.Prim.Path
import Data.Prim.Prelude

n'archive
  , n'byCreateDate
  , n'collections
  , n'photos :: Name

n'archive      = "archive"
n'byCreateDate = "byCreateDate"
n'collections  = "collections"
n'photos       = "photos"

t'archive
  , t'collections
  , t'photos :: Text

t'archive     = n'archive     ^. isoText
t'collections = n'collections ^. isoText
t'photos      = n'photos      ^. isoText

s'byCreateDate
  , s'collections
  , s'photos :: String

s'byCreateDate = n'byCreateDate ^. isoString
s'collections  = n'collections  ^. isoString
s'photos       = n'photos       ^. isoString


p'archive
  , p'collections
  , p'byCreateDate
  , p'photos :: Path

p'archive      = mkPath n'archive
p'collections  = p'archive     `snocPath` n'collections
p'byCreateDate = p'collections `snocPath` n'byCreateDate
p'photos       = p'collections `snocPath` n'photos

ps'collections
  , ps'byCreateDate
  , ps'photos
  , ps'assets
  , ps'icons
  , ps'iconsgen
  , ps'javascript
  , ps'css
  , ps'blank :: FilePath

ps'collections  = p'collections  ^. isoString
ps'byCreateDate = p'byCreateDate ^. isoString
ps'photos       = p'photos       ^. isoString
ps'assets       = "/assets"
ps'icons        = ps'assets </> "icons"
ps'iconsgen     = ps'icons  </> "generated"
ps'blank        = ps'icons  </> "blank.jpg"
ps'javascript   = ps'assets </> "javascript"
ps'css          = ps'assets </> "css"
