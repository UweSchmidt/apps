{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Main
where

import Catalog.RunImgAction
import           Catalog.Cmd
import           Catalog.Sync
import           Catalog.Rules
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImageTree
import           Data.RefTree
import qualified System.Posix as X
import Catalog.System.Convert
import Catalog.System.ExifTool
import Data.ImgAction
import Data.MetaData

{-}
import           Catalog.FilePath
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Function.Util
import           Data.List ({-intercalate,-} partition)
import           Data.Maybe
import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import qualified Data.Set as S
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import           Control.Arrow ((***))
-- -}

ccc :: IO (Either Msg (), ImgStore, Log)
ccc = runCmd $ do
  mountPath <- io X.getWorkingDirectory
  initImgStore "archive" "collections" (mountPath ++ "/data/photos")
  trcCmd cwPath >> trcCmd cwLs >> return ()
  saveImgStore ""

  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)
  cwSet refImg >> trcCmd cwPath >> trcCmd cwType >> return ()

  cwe <- we
  refDir1 <- mkImgDir cwe "emil"
  cwSet refDir1 >> trcCmd cwPath >> trcCmd cwType >> trcCmd cwFilePath >> return ()

  cwe' <- we
  pic1 <- mkImg cwe' "pic1"
  pic2 <- mkImg cwe' "pic2"
  trcCmd cwLs >> return ()

  cwSet pic2 >> trcCmd cwPath >> trcCmd cwType >> trcCmd cwLs >> return ()
  cwe'' <- we
  (mkImg cwe'' "xxx" >> return ()) `catchError` (\ _ -> return ()) -- error

  cwRoot >> trcCmd cwType >> trcCmd cwLs >> trcCmd cwPath >> return ()
--  trcCmd (fromFilePath "/home/uwe/haskell/apps/catalog/emil") >> return ()
  saveImgStore ""
  rmImgNode pic1
  rmImgNode pic2
  rmImgNode refDir1

  syncFS refImg
  saveImgStore ""
  trc "save state to c1.json"
  saveImgStore "c1.json"
  trc "load state from c1.json"
  loadImgStore "c1.json"
  saveImgStore ""
  listImages >>= io . putStrLn
  cwListPaths >>= io . putStrLn
  cwListNames >>= io . putStrLn
  rls <- buildRules
  we >>= applyRules rls >>= runImgAction

c2 :: Cmd ()
c2 = do
  loadImgStore "c1.json"
  cwRoot
  cwSyncFS
  saveImgStore ""
  listImages  >>= io . putStrLn
  cwListPaths >>= io . putStrLn
  cwListNames >>= io . putStrLn
  rls <- buildRules
  we >>= applyRules rls >>= runImgAction

c3 :: Cmd () -> Cmd ()
c3 c = local (envTrc .~ False) $ do
  loadImgStore "c1.json"
  cwRoot
  c
  saveImgStore "c1.json"
  rls <- buildRules
  we >>= applyRules rls >>= runImgAction

runc c = runCmd (c3 c)