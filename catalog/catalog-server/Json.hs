{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Json (jsonRPC)
where

import Catalog.JsonCommands
import Catalog.Cmd
import Control.Lens
import Data.ImgNode
import Data.ImgTree
import Data.Prim

import qualified Data.Aeson as J

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
mkER t = return $ J.toJSON (ER t :: JsonRes ())

mkErrMsg :: Msg -> Cmd J.Value
mkErrMsg e = mkER $ e' ^. isoText
  where
    e' = unwords . drop 1 . words $ unMsg e

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
jsonRPC jv =
  case  J.fromJSON jv :: J.Result (Text, (Text, J.Value)) of
    J.Error e ->
      mkER $ "illegal JSON RPC call: " <> e ^. isoText
    J.Success (fct, (path, args)) -> do
      let path' = path ^. from isoText
      v <- lookupByPath path'
      case v of
        Nothing ->
          case fct of
            "isCollection" ->
              mkOK False
            _ ->
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
      jl $ \ () -> read'collection n

    -- access restrictions on a collection
    "isWriteable" ->
      jl $ \ () -> read'isWriteable n

    "isRemovable" ->
      jl $ \ () -> read'isRemovable n

    "isSortable" ->
      jl $ \ () -> read'isSortable n

    -- existence check of a collection
    -- the 1. half of the check is done in jsonRPC in the Nothing case
    "isCollection" ->
      jl $ \ () -> return $ isCOL n

    -- read the src path for a collection icon
    -- result is an url pointing to the icon src
    "iconref" ->
      jl $ \ fmt ->
             case fmt ^. isoGeoAR of
               Nothing ->
                 abort $ "iconref: wrong image geometry value: " <> show fmt
               Just geo ->
                 (^. isoText) <$> read'iconref geo i


    -- get the contents of a blog entry, already converted to HTML
    "blogcontents" ->
      jl $ \ pos ->
             read'blogcontents pos n

    -- get the contents of a blog entry, already converted to HTML
    "blogsource" ->
      jl $ \ pos ->
             read'blogsource pos n

    "saveblogsource" ->
      jl $ \ (pos, val) ->
             modify'saveblogsource pos val n

    -- compute the image ref of a collection entry
    -- for previewing the image
    "previewref" ->
      jl $ \ (pos, fmt) ->
             case fmt ^. isoGeoAR of
               Nothing ->
                 abort $ "previewref: wrong image geometry value: " <> show fmt
               Just geo ->
                 (^. isoText) <$> read'previewref pos geo n

    -- get the meta data of a collection entry
    "metadata" ->
      jl $ \ pos ->
             read'metadata pos n

    -- get the rating field of a collection entry
    "rating" ->
      jl $ \ pos ->
             read'rating pos n

    -- get the rating field of all entries in a collection
    "ratings" ->
      jl $ \ () ->
             read'ratings n

    "zipcollection" ->
      jl $ \ () ->
             read'zipcollection i n

    -- change the write protection for a list of collection entries
    "changeWriteProtected" ->
      jl $ \ (ixs, ro) ->
             modify'changeWriteProtected ixs ro n

    -- sort a collection by sequence of positions
    -- result is the new collection
    "sort" ->
      jl $ \ ixs ->
             modify'sort ixs i

    -- remove all marked images and sub-collection from a collection
    "removeFromCollection" ->
      jl $ \ ixs ->
             modify'removeFromCollection ixs i n

    -- copy marked images and collections to another collection
    "copyToCollection" ->
      jl $ \ (ixs, dPath) ->
             modify'copyToCollection ixs dPath n

    -- move marked images and collections in a source col
    -- to a dest col
    -- this is implemented as a sequence of copy and remove
    "moveToCollection" ->
      jl $ \ (ixs, dPath) ->
             modify'moveToCollection ixs dPath i n

    -- set or unset the collection image
    -- i must reference a collection, not an image
    -- pos must be an index to an ImgRef for a .jpg image
    -- nothing is returned
    "colimg" ->
      jl $ \ (sPath, pos) ->
             modify'colimg sPath pos i

    -- set or unset the collection blog text
    -- i must reference a collection, not an image
    -- pos must be an index to an ImgRef for a .md text
    -- nothing is returned
    "colblog" ->
      jl $ \ (sPath, pos) ->
             modify'colblog sPath pos i

    -- create a new collection with name nm in
    -- collection i
    "newcol" ->
      jl $ \ nm ->
             modify'newcol nm i

    -- rename a sub-collection in a given collection
    "renamecol" ->
      jl $ \ new ->
             modify'renamecol new i

    -- set meta data fields for a list of selected collection entries
    "setMetaData" ->
      jl $ \ (ixs, md) ->
             modify'setMetaData ixs md n

    -- set meta data fields for a single collection entry
    "setMetaData1" ->
      jl $ \ (i', md) ->
             modify'setMetaData1 i' md n

    -- set the rating field for a list of selected collection entries
    "setRating" ->
      jl $ \ (ixs, r) ->
             modify'setRating ixs r n

    -- set the rating field for a single collection entry
    "setRating1" ->
      jl $ \ (i', r) ->
             modify'setRating1 i' r n

    -- save a snapshot of the current image store
    -- on client side, the 1. arg must be a path to an existing node
    -- simply take p'archive ("/archive"), the root node
    "snapshot" ->
      jl $ \ cmt ->
             modify'snapshot cmt

    -- sync a subcollection of /archive/photo with filesystem
    "syncCol" ->
      jl $ \ () ->
             modify'syncCol i

    -- sync a subcollection of /archive/photo with filesystem
    "syncExif" ->
      jl $ \ () ->
             modify'syncExif i

    -- import new subcollection of a collection in /archive/photo
    "newSubCols" ->
      jl $ \ () ->
             modify'newSubCols i

    -- unimplemented operations
    _ -> mkER $ "illegal JSON RPC function: " <> fct
  where
    jl :: (FromJSON a, ToJSON b) => (a -> Cmd b) -> Cmd J.Value
    jl cmd =
      jsonLift cmd args
      `catchE`
      mkErrMsg

-- 3., 4. and 5. step: parse the extra JSON argument
-- make the call of the internal operation and
-- convert the result back to JSON

jsonLift :: (FromJSON a, ToJSON b) => (a -> Cmd b) -> J.Value -> Cmd J.Value
jsonLift cmd jv =
  case J.fromJSON jv of
    J.Error e ->
      mkER $ "illegal JSON post arg: " <> e ^. isoText
    J.Success v ->
      cmd v >>= mkOK

-- ----------------------------------------
