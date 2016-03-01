module Catalog.Cmd.Remove
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Fold
import           Catalog.Cmd.Types
import           Control.Lens
import           Control.Lens.Util
import           Data.ImageTree
import           Data.ImgAction
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.Prelude
import           System.Directory (removeFile)

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
              io $ removeFile fp
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
