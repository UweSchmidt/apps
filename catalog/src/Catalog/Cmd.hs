{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Catalog.Cmd
where

import           Control.Lens
import           Control.Lens.Util
import           Control.Monad.RWSErrorIO
import           Data.ImageStore
import           Data.ImageTree
import qualified Data.List as L
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import           Data.RefTree
import           Data.Set (Set)
import           System.FilePath -- ((</>))
-- import           Catalog.FilePath
-- import           Control.Applicative
-- import           Control.Arrow (first, (***))
-- import           Control.Lens.Util
-- import qualified Data.Aeson as J
-- import           Data.Aeson hiding (Object, (.=))
-- import qualified Data.Aeson.Encode.Pretty as J
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import           Data.Maybe
-- import           Data.Prim.CheckSum
-- import Data.ImageTree
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

-- | ref to path
id2path :: ObjId -> Cmd Path
id2path i = dt >>= go
  where
    go t = return (refPath i t)

-- | ref to type
id2type :: ObjId -> Cmd String
id2type i = getImgVal i >>= go
  where
    go e = return $ concat $
      e ^.. ( theParts  . to (const "IMG")  <>
              isImgDir  . to (const "DIR")  <>
              isImgRoot . to (const "Root") <>
              isImgCol  . to (const "COL")
            )

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


-- | process all image nodes with a monadic action

processImages :: Monoid r => (ObjId -> ImgParts -> Cmd r) -> ObjId -> Cmd r
processImages pf = process
  where
    process i = do
      trcObj i "processImgTree: process node "
      getImgVal i >>= go
      where
        go e
          | isIMG e =
              pf i (e ^. theParts)
          | isDIR e =
              mconcat <$> traverse process (e ^. theDirEntries . isoSetList)
          | isROOT e =
              process (e ^. theRootImgDir)
          | otherwise =
              return mempty

listImages :: Cmd [(Path, [Name])]
listImages = do
  r <- use (theImgTree . rootRef)
  processImages listImg r
  where
    listImg :: ObjId -> ImgParts -> Cmd [(Path, [Name])]
    listImg i ps = do
      p <- id2path i
      let pns = ps ^.. isoImgParts . traverse . theImgName
      return [(p, pns)]

formatImages :: [(Path, [Name])] -> String
formatImages = unlines . map (uncurry fmt)
  where
    fmt p ns = show p ++ ": " ++ L.intercalate ", " (map show ns)

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
