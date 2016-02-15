{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Rules
where

import           Catalog.Cmd
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.ImageTree
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Monoid ((<>))
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import           Data.RefTree
import qualified Text.Regex.XMLSchema.Generic as RE
-- (Regex, parseRegex, match, splitSubex)

data Rule' a = Rule a [a]

instance Functor Rule' where
  fmap f (Rule t ss) = Rule (f t) (map f ss)

type Rule = Rule' (ImgType, Suffix)
type Deps = Rule' Name

data GenAction = GA String (Cmd Action)

type Suffix = Name

data Action = GenCopy Path Path
            | GenExif Path [Path]
            | SyncImg ObjId
            | ActSeq Action Action
            | ActNoop

-- ----------------------------------------

deriving instance Show Rule

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

matchRule :: Rule -> ObjId -> Cmd (Maybe Deps)
matchRule r@(Rule target sources) i = dt >>= (return . go) -- (return . go)
  where
    go t
      | null ts =
          return (Rule ((t ^. theName i) <> snd target) ss')
      | timestamp ts > timestamp ss =
          mzero
      | otherwise =
          return (Rule (head ts') ss')
      where
        -- nm = t ^. theName i . name2string
        ps = t ^. theNodeVal i . theParts . isoImgParts
        ts = filter (match target) ps
        ss  = concatMap (\ sp -> filter (match sp) ps) sources

        ss' = map (^. theImgName) ss
        ts' = map (^. theImgName) ts

        -- rule2deps = fmap ((nm ++) . snd) r

        timestamp = mconcat . map (^. theImgTimeStamp)

        match (tt, tp) ip =
          (ip ^. theImgType == tt)
          &&
          (tp `isNameSuffix` (ip ^. theImgName))

applyRule :: Rule -> (Deps -> ObjId -> Cmd Action) -> ObjId -> Cmd Action
applyRule r ca i = do
  m <- matchRule r i
  case m of
    Nothing ->
      return mempty
    Just r ->
      ca r i

imgCopyRule :: Int -> Int -> ObjId -> Cmd Action
imgCopyRule w h = applyRule rule action
  where
    rule = undefined
    action (Rule tn sns) i = do
      undefined


{-}
mkAction :: Name -> (ObjId -> Name -> [Name] -> Cmd Action) -> Rule ->
            ObjId -> ImgParts -> Cmd Action
mkAction pn gen d@(Rule targetP srcPs) i iparts
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
    dep = Rule
      (IMGcopy,".*[.]" ++ show w ++ "x" ++ show h ++ "[.]jpg")
      [(IMGjpg, ".*[.]jpg")]

    gen i tn sns = do
      return ActNoop
-- -}

-- ----------------------------------------
