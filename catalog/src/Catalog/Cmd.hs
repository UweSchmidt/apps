{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd
where

import           Catalog.FilePath
import           Control.Applicative
import           Control.Arrow (first, (***))
import           Control.Lens hiding (children)
import           Control.Lens.Util
import           Control.Monad.RWSErrorIO
import qualified Data.Aeson as J
import           Data.Aeson hiding (Object, (.=))
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ImageTree
import           Data.List (intercalate, partition)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Prim.CheckSum
import           Data.Prim.Name
import Data.ImageTree
import Data.ImageStore
import           Data.Prim.PathId
import           Data.Prim.Path
import           Data.Prim.TimeStamp
import           Data.RefTree
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import qualified System.Posix as X
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)

-- ----------------------------------------

data Env = Env

instance Config Env where

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd cmd = runAction cmd Env emptyImgStore

-- ----------------------------------------

withCWN :: (ObjId -> Cmd a) -> Cmd a
withCWN cmd = use theWD >>= cmd

withImgTree :: (ImgTree -> Cmd a) -> Cmd a
withImgTree cmd = use theImgTree >>= cmd

liftE :: Except String a -> Cmd a
liftE cmd =
  case runExcept cmd of
    Left  msg -> abort msg
    Right res  -> return res

trcObj :: ObjId -> String -> Cmd ()
trcObj r msg =
  withImgTree $ \ t ->
    trc $ msg ++ " " ++ show (refPath r t)

trcCmd :: Show a => Cmd a -> Cmd a
trcCmd cmd
  = do res <- cmd
       trc $ "cmd: res = " ++ show res
       return res

-- ----------------------------------------
