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
import           Catalog.Html.Photo2 (buildImgPath, colImgRef)
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
mkER t = return $ J.toJSON $ (ER t :: JsonRes ())

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

    -- read a whole collection
    "collection" ->
      jl $ \ () ->
      return n

    -- read the src path for a collection icon
    -- result is an url pointing to the icon src
    "iconref" ->
      jl $ \ fmt ->
      (^. isoText) <$> iconImgRef (fmt ^. isoGeoAR) i

    "previewref" ->
      jl $ \ (pos, fmt) ->
      (^. isoText) <$> previewImgRef pos (fmt ^. isoGeoAR) n

    "metadata" ->
      jl $ \ pos ->
      getMeta pos n

    "changeReadOnly" ->
      jl $ \ (ixs, ro) ->
      changeColReadOnlyByIxList ixs ro n

    -- sort a collection by sequence of positions
    -- result is the new collection
    "sort" ->
      jl $ \ ixs ->
        sortColByIxList ixs i

    -- remove all marked images and sub-collection from a collection
    "removeFromCollection" ->
      ( jl $ \ ixs -> do
          removeFrCol ixs i n
      )
      `catchE` mkErrMsg

    -- copy marked images and collections to another collection
    "copyToCollection" ->
      ( jl $ \ (ixs, dPath) -> do
          (di, dn) <- getIdNode' dPath
          unless (isCOL dn) $
            abort ("not a collection: " ++ show dPath)
          copyToCol ixs di n
      )
      `catchE` mkErrMsg

    -- move marked images and collections in a source col
    -- to a dest col
    -- this is implemented as a sequence of copy and remove
    "moveToCollection" ->
      ( jl $ \ (ixs, dPath) -> do
          (di, dn) <- getIdNode' dPath
          unless (isCOL dn) $
            abort ("not a collection: " ++ show dPath)
          copyToCol   ixs di   n
          removeFrCol ixs    i n
      )
      `catchE` mkErrMsg

    -- set or unset the collection image
    -- i must reference a collection, not an image
    -- nothing is returned
    "colimg" -> do
      jl $ \ ix' -> do
        setColImg ix' i n
        return ()

    -- create a new collection with name nm in
    -- collection i
    "newcol" ->
      ( jl $ \ nm -> do
          createCol nm i
      )
      `catchE` mkErrMsg

    -- rename a sub-collection in a given collection
    "renamecol" ->
      ( jl $ \ new -> do
          renameCol new i
      )
      `catchE` mkErrMsg

    "setMetaData" ->
      ( jl $ \ (ixs, md) -> do
          setMeta md ixs i n
      )
      `catchE` mkErrMsg

    -- unimplemented operations
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

removeFrCol :: [Int] -> ObjId -> ImgNode -> Cmd ()
removeFrCol ixs i n = do

  -- check whether collection is readonly
  unless (isWriteable $ n ^. theColMetaData) $ do
    path <- objid2path i
    abort ("collection is readonly: " ++ show path)

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
  processColEntry
  (\ _ _ _ -> adjustColEntries (removeAt pos) i)
  (\ ci    -> rmRec ci)

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
copyToCol ixs di n = do
  traverse_ (\ e -> copyEntryToCol di e) toBeCopied
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
  processColEntry
  (\ _ _ _ -> adjustColEntries (++ [ce]) di)
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

setColImg :: Int -> ObjId -> ImgNode -> Cmd ()
setColImg pos oid n
  | Just (iid, inm, _im) <- n ^? theColEntries . ix pos . theColImgRef =
      adjustColImg (const $ Just (iid, inm)) oid

  | otherwise =
      adjustColImg (const Nothing) oid

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
        \ ce -> do
          return (i == ce ^. theColObjId)
  let pos = fromMaybe (-1) . fmap fst $ ps

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
setMeta md ixs i n = do
  sequence_ $ zipWith3 setMeta1 [(0::Int)..] ixs (n ^. theColEntries)
  where
    setMeta1 pos mark ce
      | mark < 0 =
          return ()
      | otherwise =
          processColEntry
          (\ _ _ _ -> adjustColEntries (ix pos . theColImgRef . _3 %~ (md <>)) i)
          (adjustMetaData (md <>))
          ce

-- ----------------------------------------

getMeta :: Int -> ImgNode -> Cmd MetaData
getMeta pos n = do
  ce <- maybe
    (abort $ "illegal index in collection: " ++ show pos)
    return
    (n ^? theColEntries . ix pos)
  processColEntry
    (\ ii _ md -> (md <>) <$> getMetaData ii)
    (\ ci      -> getImgVals ci theColMetaData)
    ce

-- ----------------------------------------

addGeoToPath :: Maybe GeoAR -> Cmd FilePath -> Cmd FilePath
addGeoToPath Nothing _ =
  abort $ "wrong image geometry value"
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
  do ce <- maybe
           (abort $ "illegal index in collection: " ++ show pos)
           return
           (n ^? theColEntries . ix pos)
     processColEntry
       (\ ii nm _md -> buildImgPath ii nm)
       colImgRef
       ce

-- ----------------------------------------

changeColReadOnlyByIxList :: [Int] -> Bool -> ImgNode -> Cmd ()
changeColReadOnlyByIxList ixs ro n = do
  sequence_ $ zipWith3 markRO [(0::Int)..] ixs (n ^. theColEntries)
  where
    cf | ro        = addNoWriteAccess
       | otherwise = subNoWriteAccess
    markRO pos mark ce
      | mark < 0 =
          return ()
      | otherwise =
          processColEntry
          (\ _ _ _ -> return ())            -- ignore ImgRef's
          (adjustMetaData addNoWriteAccess)
          ce

-- ----------------------------------------
