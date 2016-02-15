{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Rules
where

import           Catalog.Cmd
import           Control.Lens hiding ((.=))
import           Data.ImageTree
import qualified Data.Map.Strict as M
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import qualified Text.Regex.XMLSchema.Generic as RE -- (Regex, parseRegex, match, splitSubex)

data Dep = Dep (ImgType, Pattern) [(ImgType, Pattern)]

data GenAction = GA String (Cmd Action)

type Pattern = String

data Action = GenCopy Path Path
            | GenExif Path [Path]
            | SyncImg Path
            | ActSeq Action Action
            | ActNoop

-- ----------------------------------------

deriving instance Show Dep

-- ----------------------------------------

instance Show GenAction where
  show (GA n _f) = show n

-- ----------------------------------------

deriving instance Show Action

instance Monoid Action where
  mempty = ActNoop

  ActNoop `mappend` a2      = a2
  a1      `mappend` ActNoop = a1
  a1      `mappend` a2      = ActSeq a1 a2

-- ----------------------------------------

mkAction :: Name -> (ObjId -> Name -> [Name] -> Cmd Action) -> Dep ->
            ObjId -> ImgParts -> Cmd Action
mkAction pn gen d@(Dep targetP srcPs) i iparts
  | null ts =
      gen i pn sns

  | timestamp ts > timestamp ss =
      return mempty

  | otherwise =
      gen i (head ts ^. theImgName) sns

  where
    ps  = iparts ^. isoImgParts
    ts  = filter (match targetP) ps
    ss  = concat $ map (\ sp -> filter (match sp) ps) srcPs
    sns = map (^. theImgName) ss

    timestamp = mconcat . map (^. theImgTimeStamp)

    match (tt, tp) ip =
      (ip ^. theImgType == tt)
      &&
      (RE.match tp (ip ^. theImgName . name2string))

genImgCopy :: Int -> Int ->
              ObjId -> ImgParts -> Cmd Action
genImgCopy w h = mkAction undefined gen dep
  where
    dep = Dep
      (IMGcopy,".*[.]" ++ show w ++ "x" ++ show h ++ "[.]jpg")
      [(IMGjpg, ".*[.]jpg")]

    gen i tn sns = do
      return ActNoop

-- ----------------------------------------
