module Catalog.Cmd.CopyRemove
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Data.ImgTree
import           Data.Prim

-- ----------------------------------------

copyCollection :: Path -> Path -> Cmd ()
copyCollection path'src path'dst = do

  -- find source and dst id
  -- abort when one of these is not there
  id'src <- fst <$> getIdNode "copyCollection: source not found"      path'src
  id'dst <- fst <$> getIdNode "copyCollection: destination not found" path'dst

  -- check whether an infinite tree would be build
  -- when copying a collection into one of its subcollections
  when (path'src `isPathPrefix` path'dst) $
    abort ("can't copy a parent collection into a subcollection")

  copyColRec id'src id'dst

copyColRec :: ObjId -> ObjId -> Cmd ()
copyColRec src dst = do
  srcVal <- getImgVal src
  unless (isCOL srcVal) $ do
    p <- objid2path src
    abort $ "copyColRec: source isn't a collection " ++ show (show p)

  name'src    <- getImgName src
  parent'src  <- getImgParent src
  parent'path <- objid2path parent'src
  dst'path    <- objid2path dst
  let editPath = substPathPrefix parent'path dst'path

  -- create empty subcollection in destination collection
  let target'path = editPath (snocPath parent'path name'src)
  void $ createCopy target'path src

  -- copy image and subcollections recursively into new dest collection
  copyEntries editPath src
  where

    createCopy :: Path -> ObjId -> Cmd ObjId
    createCopy target'path src'id = do
      col'id <- mkCollection target'path
      -- copy meta data
      md  <- getImgVals src'id theColMetaData
      adjustMetaData (const md) col'id
      return col'id

    -- copy entries copies the s collection into
    -- a destination computed by the source path and
    -- the pf path edit function
    copyEntries pf =
      foldMT imgA dirA rootA colA
      where
        imgA      _i _p      = return ()  -- NOOP
        dirA  _go _i _es _ts = return ()  -- NOOP
        rootA _go _i _d  _c  = return ()  -- NOOP

        colA go i _md im be cs _ts  = do
          dst'i  <- (mkObjId . pf) <$> objid2path i
          dst'cs <- mapM copy cs
          adjustColEntries (const dst'cs) dst'i
          adjustColImg     (const im    ) dst'i
          adjustColBlog    (const be    ) dst'i

          -- recurse into subcollections
          mapM_ go (cs ^.. traverse . theColColRef)
          where

            copy :: ColEntry -> Cmd ColEntry
            copy r@(ImgRef _i _n _m) =
              return r
            copy (ColRef i') = do
              copy'path <- pf <$> objid2path i'
              mkColColRef <$> createCopy copy'path i'

-- ----------------------------------------

removeEntry :: Path -> Cmd ()
removeEntry p = do
  i <- fst <$> getIdNode "removeEntry: entry not found " p
  rmRec i

rmRec :: ObjId -> Cmd ()
rmRec = foldMT imgA dirA rootA colA
  where
    imgA i _p = rmImgNode i

    dirA go i es _ts = do
      trc $ "dirA: " ++ show (es ^. isoDirEntries)
      mapM_ go (es ^. isoDirEntries)            -- process subdirs first
      pe <- getImgParent i >>= getImgVal        -- remode dir node
      when (not $ isROOT pe) $                  -- if it's not the top dir
        rmImgNode i

    rootA go _i dir col =
      go dir >> go col                          -- recurse into dir and col hirachy
                                                -- but don't change root
    colA go i _md _im _be cs _ts = do
      trc $ "colA: " ++ show cs
      let cs' = filter isColColRef cs           -- remove all images
      adjustColEntries (const cs') i            -- store remaining collections
      trc $ "colA: " ++ show cs'
      mapM_ go (cs' ^.. traverse . theColColRef)  -- remove the remaining collections
      pe <- getImgParent i >>= getImgVal        -- remove collection node
      when (not $ isROOT pe) $                  -- if it's not the top collection
        rmImgNode i

-- ----------------------------------------

-- traverse all collections and
-- remove entries of images not longer there
-- this is neccessary for consistent collections,
-- when a sync has been done
-- and some images have been deleted
--
-- after a sync run and before the byDate collections are
-- updated, removed images must also be removed in the collections
-- especially in the byDate colections

-- TODO: test test test

cleanupCollections :: Cmd ()
cleanupCollections = do
  trc "cleanupcollections: existence check of images referenced in collections"
  -- start with collection root
  getRootImgColId >>= cleanup
  trc "cleanupcollections: finished"
  where
    cleanup :: ObjId -> Cmd ()
    cleanup i = do
      n <- getTree (theNode i)
      case n ^. nodeVal of
        COL _md im be es _ts -> do
          cleanupIm i im
          cleanupIm i be
          cleanupEs i es
        _ ->
          return ()
      where
        cleanupIm :: ObjId -> Maybe (ObjId, Name) -> Cmd ()
        cleanupIm i' (Just (j, n)) = do
          ex <- exImg j n
          unless ex $ do
            adjustColImg (const Nothing) i'
        cleanupIm _ Nothing =
          return ()

        cleanupEs :: ObjId -> [ColEntry] -> Cmd ()
        cleanupEs i' es = do
          es' <- filterM cleanupE es
          unless (length es' == length es) $
            adjustColEntries (const es') i'
          where
            cleanupE :: ColEntry -> Cmd Bool
            cleanupE (ImgRef j n _m) = do
              exImg j n
            cleanupE (ColRef j) = do
              -- recurse into subcollection and cleanup
              cleanup j
              j'not'empty <- (not . null) <$> getImgVals j theColEntries
              -- if collection is empty, remove it
              unless j'not'empty $
                rmRec j
              return j'not'empty

        exImg :: ObjId -> Name -> Cmd Bool
        exImg i' n' = do
          me <- getTree (entryAt i')
          let ex = case me of
                Just e
                  | isIMG (e ^. nodeVal) ->
                    let ns = e ^.. nodeVal . theParts . thePartNamesI in
                    n' `elem` ns
                _ ->
                  False
          unless ex $ do
            trc  $ "exImg: image ref found in a collection for a deleted image: " ++ show (i', n')
          return ex

-- ----------------------------------------
{- }
rmGenFiles :: (ImgPart -> Bool) -> ObjId -> Cmd ()
rmGenFiles pp =
  foldMT imgA dirA rootA colA
  where
    imgA i ps = do                              -- remove the generated file(s)
      path <-  objid2path i
      runDry ("remove metadata or image copy files for " ++ show (show path)) $ do
        mapM_ (rmj path) (ps ^. isoImgParts)
        adjustImg filterJson i
      where
        rmj path part
          | pp part = do
              fp <- toFilePath (substPathName (part ^. theImgName) path)
              removeFile fp
          | otherwise =
              return ()

        filterJson pts =
          pts & isoImgParts %~ filter (not . pp)

    dirA go _i es _ts =                         -- recurse into dir entries
      mapM_ go (es ^. isoDirEntries)

    rootA go _i dir _col =                      -- recurse only into dir hierachy
      go dir

    colA _go _i _md _im _be _es _ts =                   -- noop for collections
      return ()

-- remove all JSON files containing metadata
rmJSON :: ObjId -> Cmd ()
rmJSON = rmGenFiles isJSON
  where
    isJSON p = p ^. theImgType == IMGjson

-- remove all generated image copies
rmImgCopies :: ObjId -> Cmd ()
rmImgCopies = rmGenFiles isCopy
  where
    isCopy p = p ^. theImgType == IMGcopy

-- remove image copies of a given geometry
rmImgCopy :: Geo -> ObjId -> Cmd ()
rmImgCopy g = rmGenFiles isCopy
  where
    isCopy p =
      p ^. theImgType == IMGcopy
      &&
      match (".*[.]" ++ g ^. isoString ++ "[.]jpg")
            (p ^. theImgName . isoString)
-- -}
-- ----------------------------------------
