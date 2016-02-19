{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.RunImgAction
where

import Catalog.Cmd
import Data.ImgAction
import           Control.Monad.RWSErrorIO
import           Data.Prim.Path

{-}
import           Control.Lens
import           Control.Lens.Util
import           Control.Monad.Except
import           Data.ImageStore
import           Data.ImageTree
import qualified Data.List as L
import           Data.Prim.Name
import           Data.Prim.PathId
import           Data.RefTree
import           Data.Set (Set)
import           System.FilePath -- ((</>))
import           Catalog.FilePath
import           Control.Applicative
import           Control.Arrow (first, (***))
import           Control.Lens.Util
import qualified Data.Aeson as J
import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Prim.CheckSum
import Data.ImageTree
import           Data.Prim.TimeStamp
import           System.Posix (FileStatus)
import qualified System.Posix as X
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)
-- -}

-- ----------------------------------------

runImgAction :: ImgAction -> Cmd ()
runImgAction ActNoop =
  return ()

runImgAction (ActSeq c1 c2) =
  runImgAction c1 >> runImgAction c2

runImgAction c@(GenCopy i t s ar w h) = do
  trc $ "runImgAction: " ++ show c
  p  <- id2path i
  tp <- toFilePath (substPathName t p)
  sp <- toFilePath (substPathName s p)
  execProcess
    "echo"
    [ "convert"
    , show ar
    , "-geo=" ++ show w ++ "x" ++ show h
    , sp
    , tp
    ]
    "" >>= trc
  return ()

runImgAction c@(GenMeta i t s ty) = do
  trc $ "runImgAction: " ++ show c
  p  <- id2path i
  tp <- toFilePath (substPathName t p)
  sp <- toFilePath (substPathName s p)
  execProcess
    "echo"
    [ "exiftool"
    , sp
    , tp
    , show ty
    ]
    "" >>= trc
  return ()

runImgAction c = do
  trc $ "runImgAction: not implemented: " ++ show c
  return ()

-- ----------------------------------------
