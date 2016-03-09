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
       , theWE
       , mkImgStore
       , emptyImgStore
       )
where

import           Control.Lens
import           Data.ImgTree
import           Data.Prim

import qualified Data.Aeson as J

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
  parseJSON = J.withObject "ImgStore'" $ \ o ->
    IS
    <$> o J..: "ImgTree"
    <*> o J..: "MountPath"
    <*> o J..: "CWD"

theImgTree :: Lens' ImgStore ImgTree
theImgTree k (IS t p w) = (\new -> IS new p w) <$> k t

theMountPath :: Lens' ImgStore FilePath
theMountPath k (IS t p w) = (\new -> IS t new w) <$> k p

theWE :: Lens' ImgStore ObjId
theWE k (IS t p w) = (\new -> IS t p new) <$> k w

-- ----------------------------------------

mkImgStore :: ImgTree -> FilePath -> ObjId -> ImgStore
mkImgStore = IS

emptyImgStore :: ImgStore
emptyImgStore
  = IS r "" (r ^. rootRef)
  where
    r = mkDirRoot mkObjId "" emptyImgRoot


-- ----------------------------------------
