{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.ImageStore
       ( ImgStore
       , ImgStore'
       , theImgTree
       , theMountPath
       , theWE
       , mkImgStore
       , emptyImgStore
       , mapImgStore
       )
where

import           Control.Lens
import           Data.ImgTree
import           Data.Prim

import qualified Data.Aeson as J

-- ----------------------------------------

data ImgStore' ref = IS !(DirTree ImgNode' ref) !FilePath !ref

type ImgStore  = ImgStore' ObjId

deriving instance (Show ref) => Show (ImgStore' ref)

instance (ToJSON ref) => ToJSON (ImgStore' ref) where
  toJSON (IS i mp wd) = J.object
    [ "ImgTree"   J..= i
    , "MountPath" J..= mp
    , "CWD"       J..= wd
    ]

instance (FromJSON ref, Ord ref) => FromJSON (ImgStore' ref) where
  parseJSON = J.withObject "ImgStore'" $ \ o ->
    IS
    <$> o J..: "ImgTree"
    <*> o J..: "MountPath"
    <*> o J..: "CWD"

theImgTree :: Lens' (ImgStore' ref) (DirTree ImgNode' ref)
theImgTree k (IS t p w) = (\new -> IS new p w) <$> k t
{-# INLINE theImgTree #-}

theMountPath :: Lens' (ImgStore' ref) FilePath
theMountPath k (IS t p w) = (\new -> IS t new w) <$> k p
{-# INLINE theMountPath #-}

theWE :: Lens' (ImgStore' ref) ref
theWE k (IS t p w) = (\new -> IS t p new) <$> k w
{-# INLINE theWE #-}

-- almost a functor, the Ord constraint is the problem
mapImgStore :: (Ord ref') => (ref -> ref') -> ImgStore' ref -> ImgStore' ref'
mapImgStore f (IS i mp wd) =
  IS (mapRefTree f i) mp (f wd)
{-# INLINE mapImgStore #-}

-- ----------------------------------------

mkImgStore :: ImgTree -> FilePath -> ObjId -> ImgStore
mkImgStore = IS
{-# INLINE mkImgStore #-}

emptyImgStore :: ImgStore
emptyImgStore =
  IS r "" (r ^. rootRef)
  where
    r = mkDirRoot mkObjId "" emptyImgRoot
{-# INLINE emptyImgStore #-}

-- ----------------------------------------
