{-# LANGUAGE StandaloneDeriving #-}

module Data.ImgAction
where

import Data.Prim

-- ----------------------------------------

data ImgAction = GenCopy ObjId Name Name GeoAR
               | GenMeta ObjId Name Name ImgType
               | SyncImg ObjId
               | ActSeq ImgAction ImgAction
               | ActNoop

-- ----------------------------------------

deriving instance Show ImgAction

instance Monoid ImgAction where
  mempty = ActNoop

  ActNoop `mappend` a2      = a2
  a1      `mappend` ActNoop = a1
  a1      `mappend` a2      = ActSeq a1 a2

-- ----------------------------------------
