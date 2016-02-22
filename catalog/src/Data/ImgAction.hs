{-# LANGUAGE StandaloneDeriving #-}

module Data.ImgAction
where

import Data.ImageTree
import Data.Prim.Name
import Data.Prim.PathId

-- ----------------------------------------

data ImgAction = GenCopy ObjId Name Name AspectRatio Int Int
               | GenMeta ObjId Name Name ImgType
               | SyncImg ObjId
               | ActSeq ImgAction ImgAction
               | ActNoop

data AspectRatio = Fix | Pad | Crop
                 deriving (Eq, Show)

type Geo = (Int, Int)

-- ----------------------------------------

deriving instance Show ImgAction

instance Monoid ImgAction where
  mempty = ActNoop

  ActNoop `mappend` a2      = a2
  a1      `mappend` ActNoop = a1
  a1      `mappend` a2      = ActSeq a1 a2

-- ----------------------------------------
