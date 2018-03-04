{-# LANGUAGE OverloadedStrings #-}

module Catalog.JsonCommands
  ( modify'changeWriteProtected
  , modify'colblog
  , modify'colimg
  , modify'copyToCollection
  , modify'moveToCollection
  , modify'newSubCols
  , modify'newcol
  , modify'removeFromCollection
  , modify'renamecol
  , modify'saveblogsource
  , modify'setMetaData
  , modify'setMetaData1
  , modify'setRating
  , modify'setRating1
  , modify'snapshot
  , modify'sort
  , modify'syncCol
  , modify'syncExif
  , read'blogcontents
  , read'blogsource
  , read'collection
  , read'iconref
  , read'isCollection
  , read'isRemovable
  , read'isSortable
  , read'isWriteable
  , read'metadata
  , read'previewref
  , read'rating
  , read'ratings
  , read'zipcollection
  )
where

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.MetaData

import Catalog.Cmd
import Catalog.Html.Basic ( colImgRef
                          , getColBlogSource
                          , putColBlogSource
                          , getColBlogCont
                          )
import Catalog.Sync       ( syncDirP
                          , syncNewDirs
                          )
import Catalog.System.ExifTool
                          ( getMetaData
                          , forceSyncAllMetaData
                          )
import Catalog.Zip        ( zipCollection1 )

-- ----------------------------------------
--
-- commands for modifying the catalog


modify'saveblogsource :: Int -> Text -> ImgNode -> Cmd ()
modify'saveblogsource pos t n = putBlogCont t pos n
  where
    putBlogCont :: Text -> Int -> ImgNode -> Cmd ()
    putBlogCont val =
      processColEntryAt
        (\ i nm -> putColBlogSource val i nm)
        (\ i    -> do
            n'            <- getImgVal i -- theColBlog
            ImgRef bi bn  <- maybe
                             ( do p <- objid2path i
                                  abort ("modify'saveblogsource: "
                                         ++ "no blog entry set in collection: "
                                         ++ p ^. isoString)
                             )
                             return
                             (n' ^? theColBlog . traverse)
            putColBlogSource val bi bn
        )

-- --------------------
--
-- change the write protection for a list of collection entries

modify'changeWriteProtected :: [Int] -> Bool -> ImgNode -> Cmd ()
modify'changeWriteProtected ixs ro n =
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

-- --------------------
--
-- sort a collection by sequence of positions
-- result is the new collection

modify'sort :: [Int] -> ObjId -> Cmd ()
modify'sort ixs i
  | null ixs =
      return ()
  | otherwise =
      adjustColEntries (reorderCol ixs) i

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

-- --------------------
--
-- remove all marked images and sub-collection from a collection

modify'removeFromCollection :: [Int] -> ObjId -> ImgNode -> Cmd ()
modify'removeFromCollection ixs i n =
  removeFromCol ixs i n

removeFromCol :: [Int] -> ObjId -> ImgNode -> Cmd ()
removeFromCol ixs i n = do

  -- check whether collection is readonly
  unless (isWriteable $ n ^. theColMetaData) $ do
    path <- objid2path i
    abort ("removeFrCol: collection is write protected: " ++ show path)

  traverse_ (uncurry (removeEntryFromCol i)) toBeRemoved
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

removeEntryFromCol :: ObjId -> Int -> ColEntry -> Cmd ()
removeEntryFromCol i pos =
  colEntry
  (\ _ _ -> adjustColEntries (removeAt pos) i)
  rmRec

-- --------------------
--
-- copy marked images and collections to another collection

modify'copyToCollection :: [Int] -> Path -> ImgNode -> Cmd ()
modify'copyToCollection ixs dPath n = do
  di <- checkWriteableCol dPath
  copyToCol ixs di n

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

      -- remove the access restrictions in copied collection,
      -- in a copied collection there aren't any access restrictions
      --
      -- the path of the copied collection
      let tp = dp `snocPath` (sp ^. viewBase . _2)
      modifyMetaDataRec clearAccess tp

modifyMetaDataRec :: (MetaData -> MetaData) -> Path -> Cmd ()
modifyMetaDataRec mf path = do
  i <- fst <$> getIdNode "modifyMetaDataRec: entry not found" path
  foldCollections
    ( \ go i' _md _im _be cs -> do
        adjustMetaData mf i'
        mapM_ go (cs ^.. traverse . theColColRef)
    ) i

checkWriteableCol :: Path -> Cmd ObjId
checkWriteableCol dPath = do
  (di, dn) <- getIdNode' dPath
  unless (isCOL dn) $
    abort ("jsonCall: not a collection: " ++ show dPath)
  unless (isWriteable $ dn ^. theColMetaData) $
    abort ("jsonCall: collection is write protected: " ++ show dPath)
  return di

-- --------------------
--
-- move marked images and collections in a source col
-- to a dest col
-- this is implemented as a sequence of copy and remove

modify'moveToCollection :: [Int] -> Path -> ObjId -> ImgNode -> Cmd ()
modify'moveToCollection ixs dPath i n = do
  di <- checkWriteableCol dPath
  copyToCol     ixs di n
  removeFromCol ixs  i n

-- --------------------
--
-- set or unset the collection image
-- i must reference a collection, not an image
-- pos must be an index to an ImgRef for a .jpg image

modify'colimg :: Path -> Int -> ObjId -> Cmd ()
modify'colimg = modifyCol adjustColImg

-- set or unset the collection blog text
-- i must reference a collection, not an image
-- pos must be an index to an ImgRef for a .md text

modify'colblog :: Path -> Int -> ObjId -> Cmd ()
modify'colblog = modifyCol adjustColBlog


modifyCol :: ((Maybe ImgRef -> Maybe ImgRef) -> ObjId -> Cmd ())
          -> Path -> Int -> ObjId -> Cmd ()
modifyCol adjust sPath pos i
  | pos < 0 =
      adjust (const Nothing) i
  | otherwise = do
      scn <- snd <$> getIdNode' sPath
      processColImgEntryAt
        (\ iid inm -> adjust (const $ Just (ImgRef iid inm)) i)
        pos scn

-- --------------------
--
-- create a new collection with name nm in
-- collection i

modify'newcol :: Name -> ObjId -> Cmd ()
modify'newcol nm i = do
  path  <- objid2path i
  _newi <- mkCollection (path `snocPath` nm)
  return ()

-- --------------------
--
-- rename a sub-collection in a given collection

modify'renamecol :: Name -> ObjId -> Cmd ()
modify'renamecol newName i = do
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

-- --------------------
--
-- set meta data fields for a list of selected collection entries

modify'setMetaData :: [Int] -> MetaData -> ImgNode -> Cmd ()
modify'setMetaData ixs md n =
  sequence_ $ zipWith setMeta' ixs (n ^. theColEntries)
  where
    setMeta' mark ce
      | mark < 0 =
          return ()
      | otherwise =
          colEntry
          (\ ii _ -> adjustMetaData (md <>) ii)
          (          adjustMetaData (md <>)   )
          ce

-- set meta data fields for a single collection entry

modify'setMetaData1 :: Int -> MetaData -> ImgNode -> Cmd ()
modify'setMetaData1 i' =
  modify'setMetaData ixs
  where
    ixs = replicate i' (0-1) ++ [1]

-- set the rating field for a list of selected collection entries

modify'setRating :: [Int] -> Rating -> ImgNode -> Cmd ()
modify'setRating ixs r =
  modify'setMetaData ixs (mkRating r)

-- set the rating field for a single collection entry

modify'setRating1 :: Int -> Rating -> ImgNode -> Cmd ()
modify'setRating1 i' =
  modify'setRating ixs
  where
    ixs = replicate i' (0-1) ++ [1]

-- --------------------
--

-- save a snapshot of the current image store
-- on client side, the 1. arg must be a path to an existing node
-- simply take p'archive ("/archive"), the root node

modify'snapshot :: Text -> Cmd ()
modify'snapshot t = snapshotImgStore (t ^. isoString)

-- --------------------
--
-- sync a subcollection of /archive/photo with filesystem

modify'syncCol :: ObjId -> Cmd ()
modify'syncCol = syncCol' syncDirP

syncCol' :: (Path -> Cmd ()) -> ObjId -> Cmd ()
syncCol' sync i = do
  path <- objid2path i
  unless (isPathPrefix p'photos path) $
    abort ("syncCol': collection does not have path prefix "
           ++ quotePath p'photos ++ ": "
           ++ quotePath path)
  let path'dir = substPathPrefix p'photos p'arch'photos path
  verbose $ "syncCol': directory " ++ quotePath path'dir
  sync path'dir

-- sync a subcollection of /archive/photo with filesystem

modify'syncExif :: ObjId -> Cmd ()
modify'syncExif = forceSyncAllMetaData

-- import new subcollection of a collection in /archive/photo

modify'newSubCols :: ObjId -> Cmd ()
modify'newSubCols = syncCol' syncNewDirs

-- ----------------------------------------
--
-- command for quering the catalog

-- read a whole collection

read'collection :: ImgNode -> Cmd ImgNodeP
read'collection n = mapObjId2Path n

-- access restrictions on a collection

read'isWriteable :: ImgNode -> Cmd Bool
read'isWriteable n = return (isWriteable  $ n ^. theColMetaData)

read'isRemovable :: ImgNode -> Cmd Bool
read'isRemovable n = return (isRemovable  $ n ^. theColMetaData)

read'isSortable :: ImgNode -> Cmd Bool
read'isSortable n = return (isSortable  $ n ^. theColMetaData)

-- --------------------
--
-- existence check of a collection

read'isCollection :: Path -> Cmd Bool
read'isCollection p = do
  v <- lookupByPath p
  case v of
    Nothing ->
      return False
    Just (_i, n) ->
      return $ isCOL n

-- --------------------
--
-- read the src path for a collection icon
-- result is an url pointing to the icon src

read'iconref :: GeoAR -> ObjId -> Cmd FilePath
read'iconref geo i =
  (("/" ++ geo ^. isoString) ++) <$> colImgRef i

-- --------------------
--
-- get the contents of a blog entry, already converted to HTML

read'blogcontents :: Int -> ImgNode -> Cmd Text
read'blogcontents =
  processColEntryAt
    getColBlogCont                        -- ImgEnt: entry is a blog text
    (\ i    -> do                         -- ColRef: lookup the col blog ref
        n <- getImgVal i -- theColBlog
        maybe (return mempty)             -- return nothing, when not there
              (\(ImgRef i nm) -> getColBlogCont i nm)    -- else generate the HTML
              (n ^? theColBlog . traverse)
    )

-- get the contents of a blog entry

read'blogsource :: Int -> ImgNode -> Cmd Text
read'blogsource =
  processColEntryAt
    getColBlogSource
    (\ i    -> do
        n  <- getImgVal i
        ImgRef bi bn <- maybe
              ( do p <- objid2path i
                   abort ("getBlogCont: no blog entry set in collection: "
                           ++ p ^. isoString)
              )
              return
              (n ^? theColBlog . traverse)
        getColBlogSource bi bn
    )

-- --------------------
--
-- compute the image ref of a collection entry
-- for previewing the image

read'previewref :: Int -> GeoAR -> ImgNode -> Cmd FilePath
read'previewref pos geo n =
  (("/" ++ geo ^. isoString) ++) <$>
  processColEntryAt
    buildImgPath
    colImgRef
    pos n

-- --------------------
--
-- get the meta data of a collection entry

read'metadata :: Int -> ImgNode -> Cmd MetaData
read'metadata =
  processColEntryAt
    (\ i _ -> do exifMD <- getMetaData i               -- exif meta data
                 imgMD  <- getImgVals  i theMetaData   -- title, comment, ...
                 return $ imgMD <> exifMD
    )
    (\ i   -> getImgVals i theMetaData)

-- get the rating field of a collection entry

read'rating :: Int -> ImgNode -> Cmd Rating
read'rating pos n =
  getRating <$>
  processColEntryAt (\ i' _ -> getM i') getM pos n
  where
    getM i' = getImgVals i' theMetaData

-- get the rating field of all entries in a collection

read'ratings :: ImgNode -> Cmd [Rating]
read'ratings n =
  traverse f (n ^. theColEntries)
  where
    f = colEntry (\ i' _ -> getR i') getR
      where
        getR i' = getRating <$> getImgVals i' theMetaData

-- zip all .jpg images of a collection into a zip archive
-- and return the archive path

read'zipcollection :: GeoAR -> ObjId -> ImgNode -> Cmd FilePath
read'zipcollection = zipCollection1

-- ----------------------------------------
