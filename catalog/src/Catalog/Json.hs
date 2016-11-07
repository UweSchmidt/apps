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
import           Catalog.Html.Photo2 ( buildImgPath
                                     , colImgRef
                                     , getColBlogCont
                                     , getColBlogSource
                                     , putColBlogSource
                                     )
import           Catalog.System.ExifTool (getMetaData)
-- import           Catalog.Journal
-- import           Catalog.Cmd.Types
import           Control.Lens
-- import           Control.Monad.Except
-- import           Control.Monad.RWSErrorIO
-- import           Data.Prim
import           Data.ImageStore
import           Data.ImgNode
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim
-- import           Data.Foldable
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
      jl $ \ () -> mapObjId2Path n  -- return n

    -- access restrictions on a collection
    "isWriteable" ->
      jl $ \ () -> return (isWriteable $ n ^. theColMetaData)

    "isRemovable" ->
      jl $ \ () -> return (isRemovable $ n ^. theColMetaData)

    "isSortable" ->
      jl $ \ () -> return (isSortable $ n ^. theColMetaData)

    -- read the src path for a collection icon
    -- result is an url pointing to the icon src
    "iconref" ->
      jl $ \ fmt ->
             (^. isoText) <$> iconImgRef (fmt ^. isoGeoAR) i

    -- get the contents of a blog entry, already converted to HTML
    "blogcontents" ->
      jl $ \ pos ->
             getBlogContHtml pos n

    -- get the contents of a blog entry, already converted to HTML
    "blogsource" ->
      jl $ \ pos ->
             getBlogCont pos n

    "saveblogsource" ->
      jl $ \ (pos, val) ->
             putBlogCont val pos n

    -- compute the image ref of a collection entry
    -- for previewing the image
    "previewref" ->
      jl $ \ (pos, fmt) ->
             (^. isoText) <$> previewImgRef pos (fmt ^. isoGeoAR) n

    -- get the meta data of a collection entry
    "metadata" ->
      jl $ \ pos ->
             getMeta pos n

    -- change the write protection for a list of collection entries
    "changeWriteProtected" ->
      jl $ \ (ixs, ro) ->
             changeColWriteProtectedByIxList ixs ro n

    -- sort a collection by sequence of positions
    -- result is the new collection
    "sort" ->
      jl $ \ ixs ->
             sortColByIxList ixs i

    -- remove all marked images and sub-collection from a collection
    "removeFromCollection" ->
      jl $ \ ixs ->
             removeFrCol ixs i n

    -- copy marked images and collections to another collection
    "copyToCollection" ->
      jl $ \ (ixs, dPath) ->
             do
               di <- checkWriteableCol dPath
               copyToCol ixs di n

    -- move marked images and collections in a source col
    -- to a dest col
    -- this is implemented as a sequence of copy and remove
    "moveToCollection" ->
      jl $ \ (ixs, dPath) ->
             do
               di <- checkWriteableCol dPath
               copyToCol   ixs di   n
               removeFrCol ixs    i n

    -- set or unset the collection image
    -- i must reference a collection, not an image
    -- pos must be an index to an ImgRef for a .jpg image
    -- nothing is returned
    "colimg" ->
      jl $ \ (sPath, pos) ->
             void $ setColImg sPath pos i

    -- set or unset the collection blog text
    -- i must reference a collection, not an image
    -- pos must be an index to an ImgRef for a .md text
    -- nothing is returned
    "colblog" ->
      jl $ \ (sPath, pos) ->
             void $ setColBlog sPath pos i

    -- create a new collection with name nm in
    -- collection i
    "newcol" ->
      jl $ \ nm ->
             createCol nm i

    -- rename a sub-collection in a given collection
    "renamecol" ->
      jl $ \ new ->
             renameCol new i

    "setMetaData" ->
      jl $ \ (ixs, md) ->
             setMeta md ixs i n

    -- save a snapshot of the current image store
    -- on client side, the 1. arg must be a path to an existing node
    -- simply take p'archive ("/archive"), the root node
    "snapshot" ->
      jl $ \ () ->
             snapshotImgStore

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

checkWriteableCol :: Path -> Cmd ObjId
checkWriteableCol dPath = do
  (di, dn) <- getIdNode' dPath
  unless (isCOL dn) $
    abort ("jsonCall: not a collection: " ++ show dPath)
  unless (isWriteable $ dn ^. theColMetaData) $
    abort ("jsonCall: collection is write protected: " ++ show dPath)
  return di

-- ----------------------------------------

removeFrCol :: [Int] -> ObjId -> ImgNode -> Cmd ()
removeFrCol ixs i n = do

  -- check whether collection is readonly
  unless (isWriteable $ n ^. theColMetaData) $ do
    path <- objid2path i
    abort ("removeFrCol: collection is write protected: " ++ show path)

  traverse_ (uncurry (removeEntryFrCol i)) toBeRemoved
  where
    -- elements are removed from the end to the front
    -- else the positions had to be adjusted after each remove
    toBeRemoved :: [(Int, ColEntry)]
    toBeRemoved =
      reverse
      . map snd
      . filter ((>= 0) . fst)
      . zip ixs
      . zip [0..]
      $ n ^. theColEntries

removeEntryFrCol :: ObjId -> Int -> ColEntry -> Cmd ()
removeEntryFrCol i pos =
  colEntry
  (\ _ _ -> adjustColEntries (removeAt pos) i)
  rmRec

{- -- avoid case over constructors
removeEntryFrCol i pos (ImgRef{}) =
  adjustColEntries (removeAt pos) i
removeEntryFrCol _i _pos (ColRef ci) =
  rmRec ci
-- -}

-- ----------------------------------------
--
-- copy entries to a collection

copyToCol :: [Int] -> ObjId -> ImgNode -> Cmd ()
copyToCol ixs di n =
  traverse_ (copyEntryToCol di) toBeCopied
  where
    toBeCopied :: [ColEntry]
    toBeCopied =
      map snd
      . sortBy (compare `on` fst)
      . filter ((>= 0) . fst)
      . zip ixs
      $ n ^. theColEntries

copyEntryToCol :: ObjId -> ColEntry -> Cmd ()
copyEntryToCol di ce =
  colEntry
  (\ _ _ -> adjustColEntries (++ [ce]) di)
  copyColToCol
  ce
  where
    copyColToCol si = do
      dp <- objid2path di
      sp <- objid2path si
      copyCollection sp dp

      -- remove the access restrictions in copied collection
      -- in a copied collection there aren't any access restrictions
      --
      -- the path of the copied collection
      let tp = dp `snocPath` (sp ^. viewBase . _2)
      modifyMetaDataRec clearAccess tp

{-
copyEntryToCol di e@(ImgRef{}) = do
  adjustColEntries (++ [e]) di
copyEntryToCol di (ColRef si) = do
  dp <- objid2path di
  sp <- objid2path si
  copyCollection sp dp

  -- remove the access restrictions in copied collection
  -- in a copied collection there aren't any access restrictions
  --
  -- the path of the copied collection
  let tp = dp `snocPath` (sp ^. viewBase . _2)
  modifyMetaDataRec clearAccess tp
-- -}

{- not yet in use
modifyMetaData :: (MetaData -> MetaData) -> Path -> Cmd ()
modifyMetaData mf path = do
  i <- fst <$> getIdNode "modifyMetaData: entry not found" path
  adjustMetaData mf i
-- -}

modifyMetaDataRec :: (MetaData -> MetaData) -> Path -> Cmd ()
modifyMetaDataRec mf path = do
  i <- fst <$> getIdNode "modifyMetaDataRec: entry not found" path
  processCollections
    ( \ go i' _md _im _be cs _ts -> do
        adjustMetaData mf i'
        let cs' = filter isColColRef cs
        mapM_ go (cs' ^.. traverse . theColColRef)
    ) i

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

setColImg :: Path -> Int -> ObjId -> Cmd ()
setColImg sPath pos oid
  | pos < 0 =
      adjustColImg (const Nothing) oid
  | otherwise = do
      scn <- jsonPath2ColNode sPath
      processColImgEntryAt
        (\ iid inm -> adjustColImg (const $ Just (iid, inm)) oid)
        pos scn

-- set or unset the "blog text" of a collection
-- to one of the blog texts in the collection
-- The pos param specifies the position or, if -1, the unset op

setColBlog :: Path -> Int -> ObjId -> Cmd ()
setColBlog sPath pos oid
  | pos < 0 =
      adjustColBlog (const Nothing) oid
  | otherwise = do
      scn <- jsonPath2ColNode sPath
      processColImgEntryAt
        (\ iid inm -> adjustColBlog (const $ Just (iid, inm)) oid)
        pos scn

jsonPath2ColNode :: Path -> Cmd ImgNode
jsonPath2ColNode path = do
  v <- lookupByPath path
  case v of
    Nothing ->
      abort $ "jsonPath2Id: entry not found: " <> show path
    Just idn ->
      return $ snd idn

-- ----------------------------------------

createCol :: Name -> ObjId -> Cmd ()
createCol nm i = do
  path  <- objid2path i
  _newi <- mkCollection (path `snocPath` nm)
  return ()

-- ----------------------------------------

renameCol :: Name -> ObjId -> Cmd ()
renameCol newName i = do
  iParent <- getImgParent i

  -- duplicate collection in parent collection
  dupColRec i iParent newName

  -- find position of objid i in parent collection
  ps <- flip findFstColEntry iParent $
        \ ce -> return (i == ce ^. theColObjId)
  let pos = maybe (-1) fst ps

  -- remove i in parent collection
  rmRec i

  -- move duplicated col from the end of the col entry list to the pos of i
  adjustColEntries
    (\ cs -> let i' = last cs
             in
               insertAt pos i' $ init cs
    ) iParent

-- ----------------------------------------

setMeta :: MetaData -> [Int] -> ObjId -> ImgNode -> Cmd ()
setMeta md ixs i n =
  sequence_ $ zipWith3 setMeta1 [(0::Int)..] ixs (n ^. theColEntries)
  where
    setMeta1 pos mark ce
      | mark < 0 =
          return ()
      | otherwise =
          colEntry
          (\ ii _ -> adjustMetaData (md <>) ii)
          (\ ci   -> adjustMetaData (md <>) ci)
          ce

-- ----------------------------------------

getMeta :: Int -> ImgNode -> Cmd MetaData
getMeta =
  processColEntryAt
    (\ i _ -> do exifMD <- getMetaData i               -- exif meta data
                 imgMD  <- getImgVals  i theMetaData   -- title, comment, ...
                 return $ imgMD <> exifMD
    )
    (\ i   -> getImgVals i theMetaData)

-- ----------------------------------------

addGeoToPath :: Maybe GeoAR -> Cmd FilePath -> Cmd FilePath
addGeoToPath Nothing _ =
  abort "addGeoToPath: wrong image geometry value"
addGeoToPath (Just g) cmd =
  (ppx ++) <$> cmd
  where
    ppx = "/" ++ g ^. isoString


iconImgRef :: Maybe GeoAR -> ObjId -> Cmd FilePath
iconImgRef g i =
  addGeoToPath g $ colImgRef i

previewImgRef :: Int -> Maybe GeoAR -> ImgNode -> Cmd FilePath
previewImgRef pos g n =
  addGeoToPath g $
  processColEntryAt
    (\ ii nm -> buildImgPath ii nm)
    colImgRef
    pos n

getBlogContHtml :: Int -> ImgNode -> Cmd Text
getBlogContHtml =
  processColEntryAt
    (\ i nm -> getColBlogCont i nm)       -- ImgRef: entry is a blog text
    (\ i    -> do                         -- ColRef: lookup the col blog ref
        be <- getImgVals i theColBlog
        maybe (return mempty)             -- return nothing, when not there
              (uncurry getColBlogCont)    -- else generate the HTML
              be
    )

getBlogCont :: Int -> ImgNode -> Cmd Text
getBlogCont =
  processColEntryAt
    (\ i nm -> getColBlogSource i nm)
    (\ i    -> do
        be        <- getImgVals i theColBlog
        (bi, bn)  <- maybe
                     ( do p <- objid2path i
                          abort ("getBlogCont: no blog entry set in collection: "
                                 ++ p ^. isoString)
                     )
                     return
                     be
        getColBlogSource bi bn
    )

putBlogCont :: Text -> Int -> ImgNode -> Cmd ()
putBlogCont val =
  processColEntryAt
    (\ i nm -> putColBlogSource val i nm)
    (\ i    -> do
        be        <- getImgVals i theColBlog
        (bi, bn)  <- maybe
                     ( do p <- objid2path i
                          abort ("putBlogCont: no blog entry set in collection: "
                                 ++ p ^. isoString)
                     )
                     return
                     be
        putColBlogSource val bi bn
    )

-- ----------------------------------------

changeColWriteProtectedByIxList :: [Int] -> Bool -> ImgNode -> Cmd ()
changeColWriteProtectedByIxList ixs ro n =
  zipWithM_ markRO ixs (n ^. theColEntries)
  where
    cf | ro        = addNoWriteAccess
       | otherwise = subNoWriteAccess
    markRO mark ce
      | mark < 0 =
          return ()
      | otherwise =
          colEntry
          (\ _ _ -> return ())            -- ignore ImgRef's
          (adjustMetaData cf)
          ce

-- ----------------------------------------
