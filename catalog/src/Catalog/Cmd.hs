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
          trcObj i "genCol img"
          let res = (map (mkColImgRef i) $ sort ns)
          trcObj i $ "genCol: " ++ show res
          return res
          where
            ns = pts ^.. isoImgParts
                       . traverse
                       . is (^. theImgType . to (== IMGimg))
                       . theImgName

        -- generate a coresponding collection with all entries
        -- entries are sorted by name

        dirA go i es _ts = do
          trcObj i "genCol dir"
          p  <- objid2path i
          let cp = fp p
          trcObj i $ "genCol dir " ++ show cp
          ic <- mkColByPath cp

          -- TODO: skip update if collection is newer than imgdir

          -- set collection meta data
          md <- colMetaData ic
          adjustMetaData (md <>) ic

          -- set collection entries, sorted by name
          cs  <- concat <$> mapM go (es ^. isoSetList)
          cs' <- sortColEntries cs
          adjustColEntries (const cs') ic

          -- set time processed
          setSyncTime ic

          return [mkColColRef ic]

sortColEntries :: [ColEntry] -> Cmd [ColEntry]
sortColEntries es = do
  map fst . sortBy (compare `on` snd) <$> mapM mkC es
  where
    mkC :: ColEntry -> Cmd (ColEntry, Name)
    mkC e@(ImgRef _i n) = return (e, n)
    mkC e@(ColRef i) = do
      n <- getImgName i
      return (e, n)

mergeColEntries :: [ColEntry] -> [ColEntry] -> [ColEntry]
mergeColEntries es1 es2 =
  es1 ++ filter (`notElem` es1) es2

colMetaData :: ObjId -> Cmd MetaData
colMetaData i = do
  p <- tailPath <$> objid2path i
  let t = show p ^. isoStringText
      s = ""
      c = ""
      d = ""
  return $ md t s c d
  where
    md t s c d =
      emptyMetaData
      & metaDataAt "COL:Title"      .~ t
      & metaDataAt "COL:Subtitle"   .~ s
      & metaDataAt "COL:Comment"    .~ c
      & metaDataAt "COL:CreateDate" .~ d

mkColByPath :: Path -> Cmd ObjId
mkColByPath p = do
  trc $ "mkColByPath " ++ show p
  -- check for legal path
  when (nullPath $ tailPath p) $
    abort $ "mkColByPath: can't create collection " ++show (show p)

  mid <- lookupByPath p
  case mid of
    -- collection does not yet exist
    Nothing -> do
      let (p1, n) = p ^. viewBase
      ip <- mkColByPath p1
      trcObj ip $ "mkColByPath " ++ show p1 ++ " " ++ show n
      mkImgCol ip n

    -- entry already there
    Just (ip, vp) -> do
      unless (isCOL vp) $
        abort $ "mkColByPath: can't create collection, other entry already there " ++
                show (show p)
      return ip


-- ----------------------------------------
