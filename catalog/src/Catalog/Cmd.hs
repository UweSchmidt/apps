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
       , module Catalog.Cmd.CopyRemove
       , module Catalog.Cmd.CWN
       , module Catalog.Cmd.ArchiveCollection
       , module Catalog.System.CatalogIO
       , module Catalog.System.IO
       , module Control.Monad.RWSErrorIO
       , module Control.Monad.Except
       )
where

import           Catalog.Cmd.ArchiveCollection
import           Catalog.Cmd.Basic
import           Catalog.Cmd.CopyRemove
import           Catalog.Cmd.CWN
import           Catalog.Cmd.Fold
import           Catalog.Cmd.List
import           Catalog.Journal
import           Catalog.Cmd.Types
import           Catalog.System.IO
import           Catalog.System.CatalogIO
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImgTree
import           Data.Prim

-- ----------------------------------------

initImgStore :: Name -> Name -> FilePath -> Cmd ()
initImgStore rootName colName mountPath = do
  r <- liftE $
    mkEmptyImgRoot rootName dirName colName
  put $ mkImgStore r mPath (r ^. rootRef)
  journalChange $ InitImgStore rootName colName mountPath
  where
    dirName  = mkName $ takeFileName mountPath
    mPath    = takeDirectory mountPath

-- ----------------------------------------

invImages :: Cmd ()
invImages = do  -- TODO
  _r <- use (theImgTree . rootRef)
  return ()

-- ----------------------------------------
--
-- initialization on program start

initEnv :: IO Env
initEnv =
  return defaultEnv


initState :: Env -> IO (Either String ImgStore)
initState env = do
  (res, store, _log) <- runCmd' env $ do
    mp' <- view envMountPath
    jp' <- view envJsonArchive
    initImgStore n'archive n'collections
                 (mp' </> s'photos)
    loadImgStore jp'
  case res of
    Left msg ->
      return (Left $ show msg)
    Right () ->
      return (Right store)

-- ----------------------------------------
