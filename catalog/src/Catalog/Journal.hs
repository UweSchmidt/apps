{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Catalog.Journal where

import Data.ImgTree
import Data.MetaData
import Data.Prim

data Journal' ref = MkIMG         ref Name
                  | MkDIR         ref Name
                  | MkCOL         ref Name
                  | RmObj         ref
                  | AdjImgParts   ref ImgParts
                  | AdjDirEntries ref DirEntries
                  | AdjMetaData   ref MetaData
                  | AdjColImg     ref (Maybe (ImgRef' ref))
                  | AdjColBlog    ref (Maybe (ImgRef' ref))
                  | AdjColEntries ref [ColEntry' ref]
                  | SetSyncTime   ref TimeStamp
                  | InitImgStore  Name Name SysPath
                  | LoadImgStore  FilePath
                  | SaveImgStore  FilePath
                  | SaveBlogText  ref Name Text

deriving instance (Show ref) => Show (Journal' ref)
deriving instance Functor Journal'

type Journal = Journal' ObjId

-- ----------------------------------------
