{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Catalog.Journal where

import           Data.ImgTree
import           Data.MetaData
import           Data.Prim


data Journal' ref = MkIMG         ref Name
                  | MkDIR         ref Name
                  | MkCOL         ref Name
                  | RmObj         ref
                  | AdjImgParts   ref ImgParts
                  | AdjDirEntries ref DirEntries
                  | AdjMetaData   ref MetaData
                  | AdjColImg     ref (Maybe (ObjId, Name))
                  | AdjColBlog    ref (Maybe (ObjId, Name))
                  | AdjColEntries ref [ColEntry]
                  | SetSyncTime   ref TimeStamp
                  | InitImgStore  Name Name FilePath
                  | LoadImgStore  FilePath
                  | SaveImgStore  FilePath
                  | SaveBlogText  ref Name Text

deriving instance (Show ref) => Show (Journal' ref)
deriving instance Functor Journal'

type Journal = Journal' ObjId

mapJA :: Applicative a => (ref -> a ref') -> Journal' ref -> a (Journal' ref')
mapJA f (MkIMG r n) = MkIMG <$> f r <*> pure n
mapJA f (MkDIR r n) = MkDIR <$> f r <*> pure n
mapJA f (MkCOL r n) = MkCOL <$> f r <*> pure n
mapJA f (RmObj r)   = RmObj <$> f r
mapJA f (AdjImgParts   r p) = AdjImgParts   <$> f r <*> pure p
mapJA f (AdjDirEntries r e) = AdjDirEntries <$> f r <*> pure e
mapJA f (AdjMetaData   r m) = AdjMetaData   <$> f r <*> pure m
mapJA f (AdjColImg     r i) = AdjColImg     <$> f r <*> pure i
mapJA f (AdjColBlog    r b) = AdjColBlog    <$> f r <*> pure b
mapJA f (AdjColEntries r e) = AdjColEntries <$> f r <*> pure e
mapJA f (SetSyncTime   r t) = SetSyncTime   <$> f r <*> pure t
mapJA _ (InitImgStore  n1 n2 fn) = pure (InitImgStore n1 n2 fn)
mapJA _ (LoadImgStore  fn)       = pure (LoadImgStore fn)
mapJA _ (SaveImgStore  fn)       = pure (SaveImgStore fn)
mapJA f (SaveBlogText  r n t)   = SaveBlogText <$> f r <*> pure n <*> pure t

-- ----------------------------------------
