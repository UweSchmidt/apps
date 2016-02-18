{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Rules
where

import           Catalog.Cmd
import           Control.Lens hiding ((.=))
-- import           Control.Monad
import           Data.ImageTree
-- import qualified Data.List as L
-- import qualified Data.Map.Strict as M
-- import           Data.Monoid ((<>))
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
-- import           Data.RefTree
-- import qualified Text.Regex.XMLSchema.Generic as RE
-- (Regex, parseRegex, match, splitSubex)

data RL a = RL a a

instance Functor RL where
  fmap f (RL t s) = RL (f t) (f s)

type Pattern = RL (ImgType, Suffix)
type Deps    = RL ImgPart
type Rule    = (Pattern, Deps -> ObjId -> Cmd ImgAction)

data GenAction = GA String (Cmd ImgAction)

type Suffix = Name

data ImgAction = GenCopy Path Path
            | GenExif Path [Path]
            | SyncImg ObjId
            | ActSeq ImgAction ImgAction
            | ActNoop

-- ----------------------------------------

deriving instance Show a => Show (RL a)

-- ----------------------------------------

instance Show GenAction where
  show (GA n _f) = show n

-- ----------------------------------------

deriving instance Show ImgAction

instance Monoid ImgAction where
  mempty = ActNoop

  ActNoop `mappend` a2      = a2
  a1      `mappend` ActNoop = a1
  a1      `mappend` a2      = ActSeq a1 a2

-- ----------------------------------------

applyRules :: [Rule] -> ObjId -> Cmd ImgAction
applyRules rls i0 = processImages' (matchRules rls) i0

matchRules :: [Rule] -> ObjId -> ImgParts -> Cmd ImgAction
matchRules rs i ps = do
  mconcat <$> mapM (matchRule (ps ^. isoImgParts)) rs
  where
    matchRule pts (RL target source, act) = do
      mconcat <$> mapM apply dps
      where
        sps = filter (match source) pts
        dps = map (\ p -> RL (toTP $ toTN p) p) sps

        apply :: Deps -> Cmd ImgAction
        apply r@(RL tp sp)
          | tts == zeroTimeStamp
            ||
            tts < sts =
              act r i
          | otherwise =
              return ActNoop
          where
            tts = tp ^. theImgTimeStamp
            sts = sp ^. theImgTimeStamp

        match (st, sp) ip =
          (ip ^. theImgType == st)
          &&
          (sp `isNameSuffix` (ip ^. theImgName))

        toTP tn =
          case targetPart tn of
            [] ->
              mkImgPart tn (fst target)
            (p : _) ->
              p
          where
            targetPart n' = filter (\ p -> p ^. theImgName == n') pts

        toTN part = part ^. theImgName . to (substNameSuffix (snd source) (snd target))

mkCopyRule :: Int -> Int -> Rule
mkCopyRule w h = (rl, act)
  where
    copy'sx = mkName $ "." ++ show w ++ "x" ++ show h ++ ".jpg"

    rl = RL (IMGcopy, copy'sx) (IMGjpg, mkName ".jpg")

    act (RL tp sp) i = do
      ip <- getImgParent i
      pp <- id2path ip
      return $ GenCopy (pp `snocPath` (tp ^. theImgName))
                       (pp `snocPath` (sp ^. theImgName))

buildRules :: [Rule]
buildRules = map (uncurry mkCopyRule) [(1024,768), (160,120)]

{-}
  let ss <- filter (match source) ps
  where
    go t
      | null ts =
          return (Rule ((t ^. theName i) <> snd target) ss')
      | timestamp ts > timestamp ss =
          mzero
      | otherwise =
          return (Rule (head ts') ss')
      where
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
    Just rl ->
      ca rl i

imgCopyRule :: Int -> Int -> ObjId -> Cmd Action
imgCopyRule w h = applyRule rule action
  where
    tx   = "." ++ show w ++ x ++ show h ++ ".jpg"
    rule = Rule (IMGcopy, tx) [(IMGjpg, ".jpg")]
    action (Rule tn sns) i = do
      undefined
-- -}

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
