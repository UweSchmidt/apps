module Catalog.Cmd.CopyRemove
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Data.ImgTree
import           Data.MetaData
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

  -- copyColRec id'src id'dst
  srcName   <- getImgName id'src
  dupColRec id'src id'dst srcName

-- ----------------------------------------
--
-- copy a collection src into a collection dstParent
-- with new collection name dstName

dupColRec :: ObjId -> ObjId -> Name -> Cmd ()
dupColRec src dstParent dstName = do
  srcVal  <- getImgVal src
  srcPath <- objid2path src

  unless (isCOL srcVal) $
    abort $ "dupColRec: source isn't a collection " ++ quotePath srcPath

  dstParentVal  <- getImgVal  dstParent
  dstParentPath <- objid2path dstParent

  unless (isCOL dstParentVal) $
    abort $ "dupColRec: target isn't a collection " ++ quotePath dstParentPath

  let dstPath  = dstParentPath `snocPath` dstName
  let editPath = substPathPrefix srcPath dstPath
  void $ createColCopy dstPath src
  copyColEntries editPath src

-- ----------------------------------------
--
-- create a copy of a collection src'id at target'path
createColCopy :: Path -> ObjId -> Cmd ObjId
createColCopy target'path src'id = do
  col'id <- mkCollection target'path

  -- copy collection attributes
  n <- getImgVal src'id
  adjustMetaData (const $ n ^. theColMetaData) col'id
  adjustColImg   (const $ join $ n ^? theColImg)      col'id
  adjustColBlog  (const $ join $ n ^? theColBlog)     col'id
  return col'id


-- create a copy of all collection entries at path
-- computed by path edit function pf and source path
copyColEntries :: (Path -> Path) -> ObjId -> Cmd ()
copyColEntries pf =
      foldMT imgA dirA rootA colA
      where
        imgA      _i _p  _md = return ()  -- NOOP
        dirA  _go _i _es _ts = return ()  -- NOOP
        rootA _go _i _d  _c  = return ()  -- NOOP

        colA go i _md im be cs = do
          dst'i  <- (mkObjId . pf) <$> objid2path i
          dst'cs <- mapM copy cs
          adjustColEntries (const dst'cs) dst'i
          adjustColImg     (const im    ) dst'i
          adjustColBlog    (const be    ) dst'i

          -- recurse into subcollections
          mapM_ go (cs ^.. traverse . theColColRef)
          where

            copy :: ColEntry -> Cmd ColEntry
            copy ce =
              colEntry
              (\ _ _ -> return ce)
              (\ i'  -> do
                  copy'path <- pf <$> objid2path i'
                  mkColColRef <$> createColCopy copy'path i'
              )
              ce

-- ----------------------------------------

removeEntry :: Path -> Cmd ()
removeEntry p = do
  i <- fst <$> getIdNode "removeEntry: entry not found " p
  rmRec i

rmRec :: ObjId -> Cmd ()
rmRec = foldMT imgA dirA rootA colA
  where
    imgA i _p _md = rmImgNode i

    dirA go i es _ts = do
      trc $ "dirA: " ++ show (i, es ^. isoDirEntries)
      mapM_ go (es ^. isoDirEntries)            -- process subdirs first
      pe <- getImgParent i >>= getImgVal        -- remove dir node
      unless (isROOT pe) $                      -- if it's not the top dir
        rmImgNode i

    rootA go _i dir col =
      go dir >> go col                          -- recurse into dir and col hirachy
                                                -- but don't change root
    colA go i _md _im _be cs = do
      trc $ "colA: " ++ show cs
      let cs' = filter isColColRef cs           -- remove all images
      adjustColEntries (const cs') i            -- store remaining collections

      trc $ "colA: " ++ show cs'
      mapM_ go (cs' ^.. traverse . theColColRef) -- remove the remaining collections

      unlessM (isROOT <$> (getImgParent i >>= getImgVal)) $
        rmImgNode i                             -- remove node unless it's the top collection

-- ----------------------------------------

-- traverse all collections and
-- remove entries of images not longer there
-- this is neccessary for consistent collections,
-- when a sync has been done
-- and some images have been deleted
--
-- after a sync run and before the byDate collections are
-- updated, removed images must also be removed in the collections
-- especially in the byDate collections

cleanupColByPath :: Path -> Cmd ()
cleanupColByPath p = do
  verbose $ "cleanupColByPath: cleanup col: " ++ quotePath p
  lookupByPath p >>= maybe (return ()) (cleanupCollections . fst)

cleanupAllCollections :: Cmd ()
cleanupAllCollections =
  getRootImgColId >>= cleanupCollections

cleanupCollections :: ObjId -> Cmd ()
cleanupCollections i0 = do
  p <- objid2path i0
  trc $ "cleanupcollections: existence check of images referenced in " ++ quotePath p
  cleanup i0
  trc $ "cleanupcollections: cleanup finished in " ++ quotePath p
  where
    cleanup :: ObjId -> Cmd ()
    cleanup i = do
      n <- getTree (theNode i)
      case n ^. nodeVal of
        COL _md im be es -> do
          cleanupIm i im
          cleanupIm i be
          cleanupEs i es
        _ ->
          return ()
      where
        -- TODO: Bug, for be adjustColBlog should be called
        cleanupIm :: ObjId -> Maybe ImgRef -> Cmd ()
        cleanupIm i' (Just ir) =
          unlessM (exImg ir) $
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
            cleanupE (ImgEnt ir) =
              exImg ir
            cleanupE (ColEnt j) = do
              -- recurse into subcollection and cleanup
              cleanup j
              j'not'empty <- (not . null) <$> getImgVals j theColEntries
              -- if collection is empty, remove it
              unless j'not'empty $
                rmRec j
              return j'not'empty

        exImg :: ImgRef -> Cmd Bool
        exImg (ImgRef i' n') = do
          me <- getTree (entryAt i')
          let ex = case me of
                Just e
                  | isIMG (e ^. nodeVal) ->
                    let ns = e ^.. nodeVal . theParts . thePartNamesI in
                    n' `elem` ns
                _ ->
                  False
          unless ex $
            trc  $ "exImg: image ref found in a collection for a deleted image: "
                   ++ show (i', n')
          return ex

cleanupAllRefs :: ColEntrySet -> Cmd ()
cleanupAllRefs rs =
  getRootImgColId >>= cleanupRefs rs

cleanupRefs :: ColEntrySet -> ObjId -> Cmd ()
cleanupRefs rs i0
  | isempty rs = return ()
  | otherwise  = foldCollections colA i0
  where
    colA go i _md im be es = do
      p <- objid2path i
      cleanupIm p
      cleanupBe p
      cleanupEs p
      cleanupSubCols
      removeEmptySubCols
      return ()
      where
        cleanupSubCol :: ColEntry -> Cmd ()
        cleanupSubCol =
          colEntry (\ _ _ -> return ()) go

        cleanupIm :: Path -> Cmd ()
        cleanupIm p = maybe (return ())
          (\ (ImgRef j n) -> do
              when (removedImg j n) $ do
                warn $ "cleanupRefs: col img removed: " ++
                       quotePath p ++ ", " ++ show (i, j, n)
                adjustColImg (const Nothing) i
          ) im

        cleanupBe :: Path -> Cmd ()
        cleanupBe p = maybe (return ())
          (\ (ImgRef j n) -> do
              when (removedImg j n) $ do
                warn $ "cleanupRefs: col blog removed: " ++
                       quotePath p ++ ", " ++ show (i, j, n)
                adjustColBlog (const Nothing) i
          ) be

        cleanupEs :: Path -> Cmd ()
        cleanupEs p
          | any (`memberColEntrySet` rs) es = do
              -- some refs must be deleted
              -- only rebuild the list es if any refs must be deleted
              let es' = filter (not . (`memberColEntrySet` rs)) es
              warn $ "cleanupRefs: col entries removed: "
                     ++ quotePath p ++ ", " ++ show (i, es, es')
              adjustColEntries (const es') i
          | otherwise =
              return ()

        cleanupSubCols :: Cmd ()
        cleanupSubCols =
          mapM_ cleanupSubCol es

        removedImg j n =
          mkColImgRef j n `memberColEntrySet` rs

        removeEmptySubCols :: Cmd ()
        removeEmptySubCols = do
          -- recompute colentries, maybe modified by calls of cleanupSubCol
          es1 <- getImgVals i theColEntries
          es2 <- emptySubCols es1
          mapM_ rmRec es2

        emptySubCols :: [ColEntry] -> Cmd [ObjId]
        emptySubCols = foldM checkESC []
          where
            checkESC :: [ObjId] -> ColEntry -> Cmd [ObjId]
            checkESC res =
              colEntry (\ _ _ -> return res)
                       (\ ci -> do cn <- getImgVal ci
                                   return $
                                     if isCOL cn
                                        &&
                                        null (cn ^. theColEntries)
                                        &&
                                        isRemovable (cn ^. theMetaData)
                                     then ci : res
                                     else      res
                       )

-- ----------------------------------------
