{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
-- import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Servant
-- import System.Directory
-- import Text.Blaze.Html.Renderer.Utf8

import Control.Concurrent.QSem
import Control.Exception.Base (bracket_)
import Control.Exception (SomeException, catch) -- , try, toException)
import Control.Concurrent.MVar
import Control.Monad.ReaderStateErrIO (Msg(..))
import System.Exit (die)
import System.FilePath (FilePath)
import System.IO (hPutStrLn, stderr, hFlush)

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.MetaData
import Data.ImageStore (ImgStore)

import Catalog.Cmd
import Catalog.Options (mainWithArgs)
import Catalog.Html.Basic ( buildImgPath
                          , colImgRef
                          , getColBlogSource
                          , putColBlogSource
                          , getColBlogCont
                          )
import Catalog.Sync (syncDirP, syncNewDirs)
import Catalog.System.ExifTool (getMetaData, forceSyncAllMetaData)
import Catalog.Zip (zipCollection)

import API

-- ----------------------------------------
--
-- conversions of [Text] to Path

ttp :: (Path -> a) -> ([Text] -> a)
ttp f xs = f (listToPath xs)

ttp2 :: (a -> Path -> b) -> (a -> [Text] -> b)
ttp2 = flip . ttp . flip

ttpSnd :: ((a, Path) -> b) -> ((a, [Text]) -> b)
ttpSnd f (x, ys) = f (x, listToPath ys)

-- ----------------------------------------
--
-- adapter for catalog commands to handler

pin :: ((ObjId, ImgNode) -> a) ->
       (a -> Cmd r) ->
       (Path -> Cmd r)
pin sel cmd path = do
  i'n <- getIdNode' path
  cmd $ sel i'n

mkcmd0 :: (Cmd r -> Handler r) ->
          (Path -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd0 toHandler pcmd =
  toHandler . pcmd . listToPath

mkcmd1 :: (Cmd r -> Handler r) ->
          ((ObjId, ImgNode) -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd1 toHandler pcmd =
  toHandler . pin id pcmd . listToPath

mkcmd1i :: (Cmd r -> Handler r) ->
           (ObjId -> Cmd r) ->
           ([Text] -> Handler r)
mkcmd1i toHandler pcmd =
  toHandler . pin fst pcmd . listToPath

mkcmd1n :: (Cmd r -> Handler r) ->
           (ImgNode -> Cmd r) ->
           ([Text] -> Handler r)
mkcmd1n toHandler pcmd =
  toHandler . pin snd pcmd . listToPath

mkcmd2i :: (Cmd r -> Handler r) ->
           (a -> ObjId -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2i toHandler pcmd path args =
  toHandler . (pin fst $ pcmd args) . listToPath $ path

mkcmd2 :: (Cmd r -> Handler r) ->
          (a -> (ObjId, ImgNode) -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2 toHandler pcmd path args =
  toHandler . (pin id $ pcmd args) . listToPath $ path

mkcmd2n :: (Cmd r -> Handler r) ->
           (a -> ImgNode -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2n toHandler pcmd path args =
  toHandler . (pin snd $ pcmd args) . listToPath $ path

-- ----------------------------------------
-- the server

catalogServer :: FilePath ->
                 (forall a . Cmd a -> Handler a) ->
                 (forall a . Cmd a -> Handler a) ->
                 Server CatalogAPI
catalogServer mp runR runM =
  ( bootstrap
    :<|>
    ( assets'css
      :<|>
      assets'icons
      :<|>
      assets'javascript
    )
    :<|>
    edit
  )
  :<|>
  ( ttp2 blaze
    :<|>
    imgcopy
  )
  :<|>
  ( json'read
    :<|>
    json'modify
  )
  where
    static p = serveDirectoryWebApp (mp ++ p)

    bootstrap         = static "/bootstrap"
    assets'css        = static ps'css
    assets'icons      = static ps'icons
    assets'javascript = static ps'javascript
    edit              = static "/edit.html"

    blaze :: BlazeHTML -> Path -> Handler String
    blaze (BlazeHTML (Geo' geo)) path
      | checkExtPath ".html" path = return $ show (geo, path)
      | otherwise                 = throwError err404

    imgcopy (GeoAR' geo) =
      imgcopy'archive geo
      :<|>
      imgcopy'others  geo
      where
        imgcopy'archive = ttp2 imgcopy'
        imgcopy'others  = ttp2 imgcopy'

    imgcopy' :: GeoAR -> Path -> Handler String
    imgcopy' geo path
      | checkExtPath ".jpg" path  = return $ show (geo, path)
      | otherwise                 = throwError err404

    mkR0  = mkcmd0  runR
    -- mkR1i = mkcmd1i runR
    mkR1n = mkcmd1n runR
    mkR2i = mkcmd2i runR
    mkR2n = mkcmd2n runR

    json'read =
      mkR1n read'collection
      :<|>
      mkR1n read'isWriteable
      :<|>
      mkR1n read'isRemovable
      :<|>
      mkR1n read'isSortable
      :<|>
      mkR0  read'isCollection
      :<|>
      mkR2i read'iconref
      :<|>
      mkR2n read'blogcontents
      :<|>
      mkR2n read'blogsource
      :<|>
      mkR2n read'previewref
      :<|>
      mkR2n read'metadata
      :<|>
      mkR2n read'rating
      :<|>
      mkR1n read'ratings
      where

    mkM1  = mkcmd1  runM
    mkM1i = mkcmd1i runM
    -- mkM1n = mkcmd1n runM
    mkM2  = mkcmd2  runM
    mkM2i = mkcmd2i runM
    mkM2n = mkcmd2n runM

    json'modify =
      mkM2n (uncurry modify'saveblogsource)
      :<|>
      mkM2n (uncurry modify'changeWriteProtected)
      :<|>
      mkM2i modify'sort
      :<|>
      mkM2  modify'removeFromCollection
      :<|>
      mkM2n (uncurry modify'copyToCollection)
      :<|>
      mkM2  (uncurry modify'moveToCollection)
      :<|>
      mkM2i (uncurry modify'colimg)
      :<|>
      mkM2i (uncurry modify'colblog)
      :<|>
      mkM2i modify'newcol
      :<|>
      mkM2i modify'renamecol
      :<|>
      mkM2n (uncurry modify'setMetaData)
      :<|>
      mkM2n (uncurry modify'setMetaData1)
      :<|>
      mkM2n (uncurry modify'setRating)
      :<|>
      mkM2n (uncurry modify'setRating1)
      :<|>
      mkM2  (\t _in -> modify'snapshot t)
      :<|>
      mkM1i modify'syncCol
      :<|>
      mkM1i modify'syncExif
      :<|>
      mkM1i modify'newSubCols
      :<|>
      mkM1  (uncurry modify'zipcollection)

-- ----------------------------------------
--
-- commands for modifying the catalog
-- --------------------

-- zip all .jpg images of a collection into a zip archive
-- and return the archive path

modify'zipcollection :: ObjId -> ImgNode -> Cmd FilePath
modify'zipcollection = zipCollection

-- --------------------

modify'saveblogsource :: Int -> Text -> ImgNode -> Cmd ()
modify'saveblogsource pos t n = putBlogCont t pos n
  where
    putBlogCont :: Text -> Int -> ImgNode -> Cmd ()
    putBlogCont val =
      processColEntryAt
        (\ i nm -> putColBlogSource val i nm)
        (\ i    -> do
            be        <- getImgVals i theColBlog
            (bi, bn)  <- maybe
                         ( do p <- objid2path i
                              abort ("modify'saveblogsource: "
                                     ++ "no blog entry set in collection: "
                                     ++ p ^. isoString)
                         )
                         return
                         be
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

modify'removeFromCollection :: [Int] -> (ObjId, ImgNode) -> Cmd ()
modify'removeFromCollection ixs (i, n) =
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

modify'moveToCollection :: [Int] -> Path -> (ObjId, ImgNode) -> Cmd ()
modify'moveToCollection ixs dPath (i, n) = do
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


modifyCol :: ((Maybe (ObjId, Name) -> Maybe (ObjId, Name)) ->
               ObjId -> Cmd ()
             ) ->
             Path -> Int -> ObjId -> Cmd ()
modifyCol adjust sPath pos i
  | pos < 0 =
      adjust (const Nothing) i
  | otherwise = do
      scn <- snd <$> getIdNode' sPath
      processColImgEntryAt
        (\ iid inm -> adjust (const $ Just (iid, inm)) i)
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
-- process command line args,
-- build env, configure log command
-- and init server state

main :: IO ()
main = mainWithArgs "servant" $ \ env -> do
  -- create a semaphore for syncing log output
  sem  <- newQSem 0
  let env' = env & envLogOp .~ logCmd sem
                 & envMountPath .~ "/Users/uwe/haskell/apps/catalog/data"
                 & envJsonArchive .~ "catalog/photos.hashid.json"
                 & envPort .~ 8081

  est  <- initState env'
  either die (main' env') est
  where
    logCmd :: QSem -> (String -> IO ())
    logCmd sem s =
      bracket_ (waitQSem sem) (signalQSem sem)
      ( do hPutStrLn stderr s
           hFlush    stderr
      )

main' :: Env -> ImgStore -> IO ()
main' env st = do
  -- create MVars for the image archive state
  mvRead <- newMVar st
  mvMody <- newMVar st

  let runRead  = runReadCmd env mvRead
  let runMody  = runModyCmd env mvRead mvMody

  withStdoutLogger $ \logger -> do
    let settings = setPort (env ^. envPort) $ setLogger logger defaultSettings
    runSettings settings $
      serve (Proxy :: Proxy CatalogAPI) $
      catalogServer (env ^. envMountPath) runRead runMody

-- curl -v http://localhost:8081/bootstrap/dist/css/bootstrap-theme.css
-- curl -v http://localhost:8081/assets/javascript/html-album.js
-- curl -v http://localhost:8081/edit.html
-- curl -v http://localhost:8081/blaze-1920x1200/archive/photos.html
-- curl -v http://localhost:8081/pad-1920x1200/archive/photos.jpg
-- curl -v http://localhost:8081/pad-1920x1200/cache/assets/icons/photos.jpg

-- ----------------------------------------

-- there are 2 mvars for the image store
--
-- one for reading operations, those can run in parallel
-- so the mvar is not emptied when starting an action
--
-- the 2. is for modifying the store, those operations run sequentially
-- and at the end they update both mvars wit the new state

runReadCmd :: Env -> MVar ImgStore -> Cmd a -> Handler a
runReadCmd env mvs cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- readMVar mvs
      res <-
        ((^. _1) <$> runAction cmd env store)
        `catch`
        -- TODO this still does not catch: error "some error"
        (\ e -> return (Left . Msg . show $ (e :: SomeException)))
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> Handler a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- takeMVar mvm
      (res, new'store) <- runAction cmd env store

      -- TODO try to catch: error "some error" like in runReadCmd

      _old <- swapMVar mvr new'store
      putMVar mvm new'store
      return res

raise500 :: Msg -> Handler a
raise500 (Msg msg) =
  throwError $ err500 { errBody = msg' }
  where
    msg' = show msg ^. from isoString

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
    getColBlogCont                        -- ImgRef: entry is a blog text
    (\ i    -> do                         -- ColRef: lookup the col blog ref
        be <- getImgVals i theColBlog
        maybe (return mempty)             -- return nothing, when not there
              (uncurry getColBlogCont)    -- else generate the HTML
              be
    )

-- get the contents of a blog entry

read'blogsource :: Int -> ImgNode -> Cmd Text
read'blogsource =
  processColEntryAt
    getColBlogSource
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

-- --------------------
--
-- compute the image ref of a collection entry
-- for previewing the image

read'previewref :: (Int, GeoAR) -> ImgNode -> Cmd FilePath
read'previewref (pos, geo) n =
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

-- ----------------------------------------

newSubCols :: ObjId -> Cmd ()
newSubCols = syncCol' syncNewDirs

-- ----------------------------------------
