{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.ImageStore
       ( ImgStore
       , theImgTree
       , theMountPath
       , theWD
       , emptyImgStore
       , mkImgStore
       )
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
import           Data.Prim.PathId
import           Data.Prim.Path
import           Data.Prim.TimeStamp
import           Data.RefTree
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import qualified System.Posix as X
import           Text.Regex.XMLSchema.Generic -- (Regex, parseRegex, match, splitSubex)

-- ----------------------------------------

data ImgStore = IS !ImgTree !FilePath !ObjId

deriving instance Show ImgStore

instance ToJSON ImgStore where
  toJSON (IS i mp wd) = J.object
    [ "ImgTree"   J..= i
    , "MountPath" J..= mp
    , "CWD"       J..= wd
    ]

instance FromJSON ImgStore where
  parseJSON = withObject "ImgStore'" $ \ o ->
    IS
    <$> o .: "ImgTree"
    <*> o .: "MountPath"
    <*> o .: "CWD"

theImgTree :: Lens' ImgStore ImgTree
theImgTree k (IS t p w) = (\new -> IS new p w) <$> k t

theMountPath :: Lens' ImgStore FilePath
theMountPath k (IS t p w) = (\new -> IS t new w) <$> k p

theWD :: Lens' ImgStore ObjId
theWD k (IS t p w) = (\new -> IS t p new) <$> k w

-- ----------------------------------------

mkImgStore :: FilePath -> ImgStore
mkImgStore p0
  = IS t p (t ^. rootRef)
  where
    p = takeDirectory p0
    n = mkName $ takeFileName  p0
    t = mkImgRoot n emptyImgDir

emptyImgStore :: ImgStore
emptyImgStore = mkImgStore ""

-- ----------------------------------------
