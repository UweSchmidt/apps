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
import qualified Data.Set as S

-- ----------------------------------------

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  trc $ "saveImgStore: save state to " ++ show p
  bs <- uses id J.encodePretty
  io $ if null p
       then L.putStrLn bs
       else L.writeFile p bs

mkImg' :: ImgNode -> Name -> Cmd ObjId
mkImg' v n =
  withCWN $ \ cwn t ->
  do (d, t') <- liftE $ mkImgNode n cwn v t
     theImgTree .= t'
     trcObj d "mkImg': new image node"
     return d


mkImgDir :: Name -> Cmd ObjId
mkImgDir = mkImg' emptyImgDir

mkImg :: Name -> Cmd ObjId
mkImg = mkImg' emptyImg

-- change working entry

cwe :: ObjId -> Cmd ()
cwe r =
  withImgTree $ \ t ->
  do when (hasn't (entries . at r . _Just) t) $
       abort $ "cwe: node not found: " ++ show r
--         when (hasn't (theNodeVal r . isImgDir) t) $
--           abort $ "cd: entry isn't an image dir"
     theWE .= r

cwroot :: Cmd ()
cwroot =
  withImgTree $ \ t -> cwe (t ^. rootRef)

cweType :: Cmd String
cweType =
  withCWN $ \ cwn t ->
  return $
  concat $
  t ^.. theNodeVal cwn
      . ( theParts . to (const "IMG") <>
          isImgDir . to (const "DIR")
        )

cwePath :: Cmd Path
cwePath =
  withCWN $ \ cwn t -> return $ refPath cwn t

cweLs :: Cmd [Name]
cweLs =
  withCWN $ \ cwn t ->
  return $
  t ^. theNodeVal cwn
       . ( theParts . to M.keys <>
           theDirEntries . isoSetList . traverse
           . to (\ r -> t ^. theNode r . nodeName . to (:[]))
         )


-- ----------------------------------------


ccc = runCmd $ do
  s <- mkImgStore <$> io X.getWorkingDirectory
  put s
--  saveImgStore ""
  trcCmd cwePath
  d <- mkImgDir "emil"
  cwe d
  trcCmd cwePath
  trcCmd cweType
  i1 <- mkImg "pic1"
  i2 <- mkImg "pic2"
  trcCmd cweLs
  cwe i2
  trcCmd cweType
  trcCmd cweLs
  -- d2 <- mkImg "xxx" -- error
  cwroot
  trcCmd cweType
  trcCmd cweLs
  trcCmd cwePath
  saveImgStore ""
