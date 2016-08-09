{-# LANGUAGE OverloadedStrings #-}

module Data.Prim.Constants where

import Data.Prim.Name
import Data.Prim.Path
import Data.Prim.Prelude

n'archive
  , n'bycreatedate
  , n'collections
  , n'photos :: Name

n'archive      = "archive"
n'bycreatedate = "bycreatedate"
n'collections  = "collections"
n'photos       = "photos"

t'archive
  , t'collections
  , t'photos :: Text

t'archive     = n'archive     ^. isoText
t'collections = n'collections ^. isoText
t'photos      = n'photos      ^. isoText

s'bycreatedate
  , s'collections
  , s'photos :: String

s'bycreatedate = n'bycreatedate ^. isoString
s'collections  = n'collections  ^. isoString
s'photos       = n'photos       ^. isoString


p'archive
  , p'collections
  , p'bycreatedate
  , p'photos :: Path

p'archive      = mkPath n'archive
p'collections  = p'archive     `snocPath` n'collections
p'bycreatedate = p'collections `snocPath` n'bycreatedate
p'photos       = p'collections `snocPath` n'photos

ps'collections
  , ps'bycreatedate
  , ps'photos
  , ps'assets
  , ps'icons
  , ps'iconsgen
  , ps'javascript
  , ps'css
  , ps'blank :: FilePath

ps'collections  = p'collections  ^. isoString
ps'bycreatedate = p'bycreatedate ^. isoString
ps'photos       = p'photos       ^. isoString
ps'assets       = "/assets"
ps'icons        = ps'assets </> "icons"
ps'iconsgen     = ps'icons  </> "generated"
ps'blank        = ps'icons  </> "blank.jpg"
ps'javascript   = ps'assets </> "javascript"
ps'css          = ps'assets </> "css"

-- ----------------------------------------

-- constants for generated collections

tt'bydate
  , tt'collections :: Text

tt'bydate      = "Geordnet nach Datum"
tt'collections = "Uwe alle seine Bilder"

tt'year :: String -> Text
tt'year y = ("Bilder aus " ++ y) ^. isoText

tt'month :: String -> String -> Text
tt'month y m =
  unwords [ "Bilder aus dem"
          , de'month (read m)
          , y
          ]
  ^. isoText

tt'day :: String -> String -> String -> Text
tt'day y m d =
  unwords [ "Bilder vom"
          , show  (read d :: Int) ++ "."
          , de'month (read m)
          , y
          ]
  ^. isoText


ta'readonly :: Text
ta'readonly = "readonly"


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
