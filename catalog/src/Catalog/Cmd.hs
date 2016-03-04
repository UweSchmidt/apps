{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd
       ( module Catalog.Cmd
       , module Catalog.Cmd.Types
       , module Catalog.Cmd.Basic
       , module Catalog.Cmd.Fold
       , module Catalog.Cmd.List
       , module Catalog.Cmd.Remove
       , module Catalog.Cmd.CWN
       , module Catalog.System.IO
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.CWN
import           Catalog.Cmd.Fold
import           Catalog.Cmd.List
import           Catalog.Cmd.Remove
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Control.Lens
import           Control.Lens.Util
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImageTree
import           Data.MetaData
import           Data.Prim
import           Data.RefTree

-- ----------------------------------------

initImgStore :: Name -> Name -> FilePath -> Cmd ()
initImgStore rootName colName mountPath
  = do r <- liftE $
            mkEmptyImgRoot rootName dirName colName
       put $ mkImgStore r mPath (r ^. rootRef)
  where
    dirName  = mkName $ takeFileName mountPath
    mPath    = takeDirectory mountPath

-- ----------------------------------------

invImages :: Cmd ()
invImages = do
  _r <- use (theImgTree . rootRef)
  return ()

-- ----------------------------------------

-- gen collection for whole img hierachy

genCollectionsByDir :: Cmd ()
genCollectionsByDir = do
  ic <- getRootImgColId                 -- the collection root
  pc <- objid2path ic
  di <- getRootImgDirId

  let (rootName, pc1) = pc  ^. viewTop
  let (colName, _pc2) = pc1 ^. viewTop
  let img2col p =
        rootName `consPath` colName `consPath` tailPath p
  es <- genCol img2col di
  adjustColEntries (`mergeColEntries` es) ic
  trc $ "genCollectionsbydir: " ++ show es
  return ()
  where
    genCol :: (Path -> Path) -> ObjId -> Cmd [ColEntry]
    genCol fp =
      processImgDirs imgA dirA
      where
        -- collect all processed jpg images for a single img

        imgA i pts = do
          let res = (map (mkColImgRef i) $ sort ns)
          trcObj i $ "genCol img: " ++ show res
          return res
          where
            ns = pts ^.. isoImgParts
                       . traverse
                       . is (^. theImgType . to (== IMGjpg))
                       . theImgName

        -- generate a coresponding collection with all entries
        -- entries are sorted by name

        dirA go i es _ts = do
          p  <- objid2path i
          let cp = fp p
          trcObj i $ "genCol dir " ++ show cp
          ic <- mkColByPath cp

          dirSyncTime <- getImgVals i  theDirSyncTime
          colSyncTime <- getImgVals ic theColSyncTime

          if colSyncTime >= dirSyncTime
            then do
              -- the collection is up to date
              -- only the subdirs need to be traversed
              trcObj i "genCol dir: dir is up to date, traversing subdirs"
              cs  <- filterM (\ i' -> getImgVals i' (to isDIR)) (es ^. isoSetList)
              mapM_ (\ i' -> trcObj i' "subdir") cs
              void $ mapM go cs
            else do
              -- set collection meta data
              -- from current dir contents
              md <- colMetaData "Name" ic
              adjustMetaData (md <>) ic

              -- set collection entries, sorted by name
              cs  <- concat <$> mapM go (es ^. isoSetList)
              cs' <- sortColEntries cs
              adjustColEntries (const cs') ic

              -- set time processed
              setSyncTime ic

          return [mkColColRef ic]

-- collection entries are sorted by name

sortColEntries :: [ColEntry] -> Cmd [ColEntry]
sortColEntries es = do
  map fst . sortBy (compare `on` snd) <$> mapM mkC es
  where
    mkC :: ColEntry -> Cmd (ColEntry, Name)
    mkC ce = do
      n <- getImgName (ce ^. theColImgObjId)
      return (ce, n)
{-}
    mkC e@(ImgRef _i n) = return (e, n)
    mkC e@(ColRef i) = do
      n <- getImgName i
      return (e, n)
-- -}

-- merge old an new entries
-- old entries are removed from list of new entries
-- the remaining new entries are appended

mergeColEntries :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries es1 es2 =
  es1 ++ filter (`notElem` es1) es2


-- meta data for generated collections

colMetaData :: Text -> ObjId -> Cmd MetaData
colMetaData oby i = do
  p <- tailPath <$> objid2path i
  d <- (\ t -> show t ^. isoStringText) <$> atThisMoment
  let t = show p ^. isoStringText
      s = ""
      c = ""
  return $ md t s c d oby
  where
    md t s c d o =
      emptyMetaData
      & metaDataAt "COL:Title"      .~ t
      & metaDataAt "COL:Subtitle"   .~ s
      & metaDataAt "COL:Comment"    .~ c
      & metaDataAt "COL:CreateDate" .~ d
      & metaDataAt "COL:OrderedBy"  .~ o


-- create collections recursively, similar to 'mkdir -p'

mkColByPath :: Path -> Cmd ObjId
mkColByPath p = do
  -- trc $ "mkColByPath " ++ show p
  -- check for legal path
  when (nullPath $ tailPath p) $
    abort $ "mkColByPath: can't create collection " ++show (show p)

  mid <- lookupByPath p
  case mid of
    -- collection does not yet exist
    Nothing -> do
      let (p1, n) = p ^. viewBase
      ip <- mkColByPath p1
      -- trcObj ip $ "mkColByPath " ++ show p1 ++ " " ++ show n
      mkImgCol ip n

    -- entry already there
    Just (ip, vp) -> do
      unless (isCOL vp) $
        abort $ "mkColByPath: can't create collection, other entry already there " ++
                show (show p)
      return ip


-- ----------------------------------------
