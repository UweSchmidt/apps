{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Sync
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
import Catalog.Cmd
import           Control.Monad.Except

-- ----------------------------------------

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  trc $ "saveImgStore: save state to " ++ show p
  bs <- uses id J.encodePretty
  io $ if null p
       then L.putStrLn bs
       else L.writeFile p bs

mkImgDir :: Name -> Cmd ObjId
mkImgDir n
  = withCWD $ \ cwd ->
    withImgTree $ \ t ->
      do (d, t') <- liftE $ mkImgNode n cwd emptyImgDir t
         theImgTree .= t'
         trcObj d "mkImgDir: new dir"
         return d

cd :: ObjId -> Cmd ()
cd r
  = withImgTree $ \ t ->
      do when (hasn't (entries . at r . _Just) t) $
           abort $ "cd: image dir not found: " ++ show r
         when (hasn't (theNodeVal r . isImgDir) t) $
           abort $ "cd: entry isn't an image dir"
         trcObj r "cd: cwd is"
         theWD .= r

pwd :: Cmd ()
pwd =
  withCWD $ \ cwd ->
  withImgTree $ \ t ->
    io $ putStrLn $ show $ refPath cwd t

-- ----------------------------------------


ccc = runCmd $ do
  s <- mkImgStore <$> io X.getWorkingDirectory
  put s
  saveImgStore ""
  pwd
  d <- mkImgDir "emil"
  cd d
  pwd
  saveImgStore ""
