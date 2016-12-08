{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Constants where

import Data.Prim.Name
import Data.Prim.Path
import Data.Prim.Prelude
import qualified Data.Text as T

n'archive
  , n'albums
  , n'bycreatedate
  , n'clipboard
  , n'collections
  , n'imports
  , n'photos
  , n'trash :: Name

n'archive      = "archive"
n'albums       = "albums"
n'bycreatedate = "timeline"
n'clipboard    = "clipboard"
n'collections  = "collections"
n'imports      = "imports"
n'photos       = "photos"
n'trash        = "trash"

t'archive
  , t'collections
  , t'photos :: Text

t'archive     = n'archive     ^. isoText
t'collections = n'collections ^. isoText
t'photos      = n'photos      ^. isoText

s'bycreatedate
  , s'clipboard
  , s'collections
  , s'photos
  , s'trash :: String

s'bycreatedate = n'bycreatedate ^. isoString
s'collections  = n'collections  ^. isoString
s'photos       = n'photos       ^. isoString
s'clipboard    = n'clipboard    ^. isoString
s'trash        = n'trash        ^. isoString

p'archive
  , p'arch'photos
  , p'albums
  , p'clipboard
  , p'collections
  , p'bycreatedate
  , p'imports
  , p'photos
  , p'trash :: Path

p'archive      = mkPath n'archive
p'arch'photos  = p'archive     `snocPath` n'photos
p'collections  = p'archive     `snocPath` n'collections
p'albums       = p'collections `snocPath` n'albums
p'bycreatedate = p'collections `snocPath` n'bycreatedate
p'imports      = p'collections `snocPath` n'imports
p'photos       = p'collections `snocPath` n'photos
p'clipboard    = p'collections `snocPath` n'clipboard
p'trash        = p'collections `snocPath` n'trash

ps'collections
  , ps'bycreatedate
  , ps'cache
  , ps'clipboard
  , ps'trash
  , ps'photos
  , ps'assets
  , ps'icons
  , ps'iconsgen
  , ps'javascript
  , ps'css
  , ps'exifcache
  , ps'blank :: FilePath

ps'clipboard    = p'clipboard    ^. isoString
ps'trash        = p'trash        ^. isoString
ps'cache        = "/cache"
ps'collections  = p'collections  ^. isoString
ps'bycreatedate = p'bycreatedate ^. isoString
ps'photos       = p'photos       ^. isoString
ps'assets       = "/assets"
ps'icons        = ps'assets </> "icons"
ps'iconsgen     = ps'icons  </> "generated"
ps'blank        = ps'icons  </> "blank.jpg"
ps'javascript   = ps'assets </> "javascript"
ps'css          = ps'assets </> "css"
ps'exifcache    = ps'cache  </> "exif-meta"

-- ----------------------------------------

-- constants for generated collections

tt'albums
  , tt'bydate
  , tt'clipboard
  , tt'imports
  , tt'trash
  , tt'collections
  , tt'photos :: Text

tt'bydate      = "Geordnet nach Datum"
tt'clipboard   = "Clipboard"
tt'albums      = "Alle Alben"
tt'imports     = "Photo2 Import"
tt'trash       = "Papierkorb"
tt'collections = "Uwe alle seine Bilder"
tt'photos      = "Alle Ordner"

tt'year :: String -> Text
tt'year y = y ^. isoText

tt'month :: String -> String -> Text
tt'month y m =
  unwords [ de'month (read m)
          , y
          ]
  ^. isoText

tt'day :: String -> String -> String -> Text
tt'day y m d =
  unwords [ show  (read d :: Int) ++ "."
          , de'month (read m)
          , y
          ]
  ^. isoText

-- access restrictions

no'restr
  , no'change
  , no'delete, no'sort, no'write, no'wrtdel :: Text

no'restr  = ""
no'write  = "no-write"
no'sort   = "no-sort"
no'delete = "no-delete"
no'change = T.unwords [no'delete, no'sort, no'write]
no'wrtdel = T.unwords [no'delete, no'write]

to'colandname
  , to'dateandtime
  , to'name :: Text

to'colandname  = "colandname"
to'dateandtime = "dateandtime"
to'name        = "name"

-- ----------------------------------------

de'month :: Int -> String
de'month i = [ "Januar", "Februar", "März", "April", "Mai", "Juni"
             , "Juli", "August", "September","Oktober", "November", "Dezember"
             ] !! (i - 1)

-- ----------------------------------------
