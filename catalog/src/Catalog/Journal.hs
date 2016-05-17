{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Catalog.Journal where

-- import           Catalog.Cmd.Types
-- import           Data.ImageStore
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
                  | AdjColEntries ref [ColEntry]
                  | SetSyncTime   ref TimeStamp
                  | InitImgStore  Name Name FilePath
                  | LoadImgStore  FilePath
                  | SaveImgStore  FilePath

deriving instance (Show ref) => Show (Journal' ref)
deriving instance Functor Journal'

type Journal = Journal' ObjId
