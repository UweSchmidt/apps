module Catalog.Cmd.Remove
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Control.Lens
import           Control.Lens.Util
import           Data.RefTree
import           Data.ImageTree
import           Data.ImgAction
import           Data.Prim

-- ----------------------------------------

copyColRec :: ObjId -> ObjId -> Cmd ()
copyColRec src dst = do
  srcNode <- getTree (theNode src)
  when (not . isCOL $ srcNode ^. nodeVal) $ do
    p <- objid2path src
    abort $ "copyColRec: source isn't a collection " ++ show (show p)

  dstNode <- getTree (theNode dst)
  when (not . isCOL $ dstNode ^. nodeVal) $ do
    p <- objid2path dst
    abort $ "copyColRec: destination isn't a collection " ++ show (show p)

  parent'src  <- getImgParent src
  parent'path <- objid2path parent'src
  dst'path    <- objid2path dst
  let editPath = substPathPrefix parent'path dst'path

  -- create empty subcollection in destination dir
  -- TODO

  copyEntries editPath src
  where
    -- copy entries copies the s collection into
    -- a destination computed by the source path and
    -- the pf path edit function
    copyEntries pf s =
      foldMT imgA dirA rootA colA s
      where
        imgA      _i _p      = return ()  -- NOOP
        dirA  _go _i _es _ts = return ()  -- NOOP
        rootA _go _i _d  _c  = return ()  -- NOOP

        colA go i md cs _ts  = do
          dst'cs <- mapM copy cs
          undefined dst'cs -- TODO

          -- recurse into subcollections
          mapM_ go (cs ^.. traverse . theColColRef)
          where

            copy :: ColEntry -> Cmd ColEntry
            copy r@(ImgRef _i _n) =
              return r
            -- empty subcollection must be created
            -- in the destination collection
            copy (ColRef i) = do
              new'i <- undefined  -- TODO

              return new'i

-- ----------------------------------------

rmRec :: ObjId -> Cmd ()
rmRec = foldMT imgA dirA rootA colA
  where
    imgA i _p = rmImgNode i

    dirA go i es _ts = do
      mapM_ go (es ^. isoSetList)               -- process subdirs first
      pe <- getImgParent i >>= getImgVal        -- remode dir node
      when (not $ isROOT pe) $                  -- if it's not the top dir
        rmImgNode i

    rootA go _i dir col =
      go dir >> go col                          -- recurse into dir and col hirachy
                                                -- but don't change root
    colA go i _md cs _ts = do
      mapM_ go (cs ^.. traverse . theColColRef)
      pe <- getImgParent i >>= getImgVal        -- remove collection node
      when (not $ isROOT pe) $                  -- if it's not the top collection
        rmImgNode i

-- ----------------------------------------

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
      mapM_ go (es ^. isoSetList)

    rootA go _i dir _col =                      -- recurse only into dir hierachy
      go dir

    colA _go _i _md _es _ts =                   -- noop for collections
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
rmImgCopy (w, h) = rmGenFiles isCopy
  where
    isCopy p =
      p ^. theImgType == IMGcopy
      &&
      match (".*[.]" ++ show w ++ "x" ++ show h ++ "[.]jpg")
            (p ^. theImgName . name2string)

-- ----------------------------------------
