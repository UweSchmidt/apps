{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd
where

import Control.Lens
import Control.Monad.RWSErrorIO
import Data.ImageStore
import Data.ImageTree
import Data.Prim.Name
import Data.Prim.PathId
import Data.Prim.TimeStamp
import Data.RefTree
import Data.Set (Set)
import System.FilePath -- ((</>))
-- import           Catalog.FilePath
-- import           Control.Applicative
-- import           Control.Arrow (first, (***))
-- import           Control.Lens.Util
-- import qualified Data.Aeson as J
-- import           Data.Aeson hiding (Object, (.=))
-- import qualified Data.Aeson.Encode.Pretty as J
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import           Data.List (intercalate, partition)
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import           Data.Maybe
-- import           Data.Prim.CheckSum
-- import Data.ImageTree
-- import           Data.Prim.Path
-- import           Data.Prim.TimeStamp
-- import           System.Posix (FileStatus)
-- import qualified System.Posix as X
-- import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)

-- ----------------------------------------

data Env = Env

instance Config Env where

type Cmd = Action Env ImgStore

runCmd :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runCmd cmd = runAction cmd Env emptyImgStore

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
--
-- simple monadic ops

we :: Cmd ObjId
we = use theWE

dt :: Cmd ImgTree
dt = use theImgTree

getTree :: Getting a ImgTree a -> Cmd a
getTree l = use (theImgTree . l)

getImgName :: ObjId -> Cmd Name
getImgName i = use (theImgTree . theNode i . nodeName)

getImgParent :: ObjId -> Cmd ObjId
getImgParent i = use (theImgTree . theNode i . parentRef)

getImgVal :: ObjId -> Cmd ImgNode
getImgVal i = use (theImgTree . theNode i . nodeVal)

getImgVals :: ObjId -> Getting a ImgNode a -> Cmd a
getImgVals i l = use (theImgTree . theNode i . nodeVal . l)

withCWN :: (ObjId -> ImgTree -> Cmd a) -> Cmd a
withCWN cmd
  = do wd <- we
       t  <- dt
       cmd wd t

liftE :: Except String a -> Cmd a
liftE cmd = cmd `andThenE` return

andThenE :: Except String a -> (a -> Cmd b) -> Cmd b
andThenE cmd f =
  case runExcept cmd of
    Left  msg -> abort msg
    Right res -> f res

-- ----------------------------------------
--
-- smart constructors

mkImg' :: ImgNode -> ObjId -> Name -> Cmd ObjId
mkImg' v i n = dt >>= go
  where
    go t = do
      (d, t') <- liftE $ mkImgNode n i v t
      theImgTree .= t'
      trcObj d "mkImg': new image node"
      return d

mkImgDir :: ObjId -> Name -> Cmd ObjId
mkImgDir = mkImg' emptyImgDir

mkImg :: ObjId -> Name -> Cmd ObjId
mkImg = mkImg' emptyImg

rmImgNode :: ObjId -> Cmd ()
rmImgNode i = dt >>= go
  where
    go t = do
      t' <- liftE $ remImgNode i t
      theImgTree .= t'

adjustImg :: (ImgParts -> ImgParts) -> ObjId -> Cmd ()
adjustImg f i =
  theImgTree . theNodeVal i . theParts %= f

adjustDirEntries :: (Set ObjId -> Set ObjId) -> ObjId -> Cmd ()
adjustDirEntries f i =
  theImgTree . theNodeVal i . theDirEntries %= f

adjustDirTimeStamp :: (TimeStamp -> TimeStamp) -> ObjId -> Cmd ()
adjustDirTimeStamp f i =
  theImgTree . theNodeVal i . theDirTimeStamp %= f

-- ----------------------------------------
--
-- trace commands

trcObj :: ObjId -> String -> Cmd ()
trcObj r msg = dt >>= \ t ->
  trc $ msg ++ " " ++ show (refPath r t)

trcCmd :: Show a => Cmd a -> Cmd a
trcCmd cmd
  = do res <- cmd
       trc $ "cmd: res = " ++ show res
       return res

-- ----------------------------------------
