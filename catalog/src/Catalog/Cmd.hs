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
       , module Catalog.Cmd.ArchiveCollection
       , module Catalog.System.IO
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Catalog.Cmd.ArchiveCollection
import           Catalog.Cmd.Basic
import           Catalog.Cmd.CWN
import           Catalog.Cmd.Fold
import           Catalog.Cmd.List
import           Catalog.Cmd.Remove
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Control.Lens
-- import           Control.Lens.Util
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImageTree
-- import           Data.MetaData
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
