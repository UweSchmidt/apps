{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd.CWN
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Control.Lens
import           Control.Lens.Util
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImageTree
import           Data.ImgAction
import           Data.MetaData
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.Prelude
import           Data.Prim.TimeStamp
import           Data.RefTree
import           System.Directory (removeFile)

-- ----------------------------------------
--
-- ops on current node


we :: Cmd ObjId
we = use theWE

withCWN :: (ObjId -> ImgTree -> Cmd a) -> Cmd a
withCWN cmd
  = do wd <- we
       t  <- dt
       cmd wd t

-- change working node
cwSet :: ObjId -> Cmd ()
cwSet i = do
  e <- getTree (entryAt i)
  case e of
    Nothing ->
      abort $ "cwSet: node not found: " ++ show i
    Just _ ->
      theWE .= i

cwSetPath :: Path -> Cmd ()
cwSetPath p =
  cwSet (mkObjId p)
  `catchError`
  (\ _e -> abort $ "cwSetPath: no such node " ++ show p)

-- | change working node to root node
cwRoot :: Cmd ()
cwRoot = getTree rootRef >>= cwSet

-- | change working node to parent

cwUp :: Cmd ()
cwUp = do
  ip <- we >>= getImgParent
  theWE .= ip


cwDown :: Name -> Cmd ()
cwDown d = do
  p <- flip snocPath d <$> cwPath
  cwSetPath p

cwType :: Cmd String
cwType = we >>= objid2type

cwPath :: Cmd Path
cwPath = we >>= objid2path

-- | list names of elements in current node
cwLs :: Cmd [Name]
cwLs = we >>= objid2contNames

-- | convert working node path to file system path
cwFilePath :: Cmd FilePath
cwFilePath = cwPath >>= toFilePath

cwListPaths :: Cmd String
cwListPaths = we >>= listPaths

cwListNames :: Cmd String
cwListNames = we >>= listNames

-- ----------------------------------------
