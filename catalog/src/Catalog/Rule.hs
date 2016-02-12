{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Rule
where

import Data.Prim.Name
import Data.Prim.Path
import Data.ImageTree
import Catalog.Cmd
import qualified Data.Map.Strict as M

data Dep = Dep (ImgType, Pattern) [(ImgType, Pattern)]

data GenAction = GA String (ImgParts -> Action)

type Pattern = String

data Action = GenCopy Path Path
            | GenExif Path [Path]
            | SyncImg Path
            | ActSeq Action Action
            | ActNull

-- ----------------------------------------

deriving instance Show Dep

-- ----------------------------------------

instance Show GenAction where
  show (GA n _f) = show n

-- ----------------------------------------

deriving instance Show Action

-- ----------------------------------------

genCopy :: Dep -> GenAction
genCopy d@(Dep (tt, tp) srcs) = GA ("genImgCopy: " ++ show d) go
  where
    go (ImgParts pm) =
      undefined

genImgCopy :: Int -> Int -> GenAction
genImgCopy w h = genCopy p
  where
    p = Dep (IMGcopy,".*[.]" ++ show w ++ "x" ++ show h ++ "[.]jpg")
            [(IMGjpg, ".*[.]jpg")]

-- ----------------------------------------
