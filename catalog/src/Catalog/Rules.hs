{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Rules
where

import Catalog.Cmd
import Control.Lens
import Data.ImageTree
import Data.ImgAction
import Data.Prim.Name
import Data.Prim.PathId
import Data.Prim.TimeStamp
import Catalog.FilePath

{-}
-- import           Data.Prim.Path
-- import qualified Data.List as L
-- import qualified Data.Map.Strict as M
-- import           Data.Monoid ((<>))
-- import           Data.RefTree
-- import qualified Text.Regex.XMLSchema.Generic as RE
-- (Regex, parseRegex, match, splitSubex)
-- -}

-- ----------------------------------------

data RL a = RL a a

instance Functor RL where
  fmap f (RL t s) = RL (f t) (f s)

data Pattern = PT NameImgType ImgType
data Deps    = DP ImgPart ImgPart
type Rule    = (Pattern, Deps -> ObjId -> Cmd ImgAction)

-- ----------------------------------------

deriving instance Show a => Show (RL a)

-- ----------------------------------------

applyRules :: [Rule] -> ObjId -> Cmd ImgAction
applyRules rls i0 = processImages (matchRules rls) i0

matchRules :: [Rule] -> ObjId -> ImgParts -> Cmd ImgAction
matchRules rs i ps = do
  mconcat <$> mapM (matchRule (ps ^. isoImgParts)) rs
  where
    matchRule pts (PT target source, act) = do
      mconcat <$> mapM apply dps
      where
        sps = filter (not . nullName . snd) $
              map (match source) pts
        dps = map (\ (p, e) -> DP (toTP $ toTN p e) p) sps

        apply :: Deps -> Cmd ImgAction
        apply r@(DP tp sp)
          | tts == zeroTimeStamp
            ||
            tts < sts =
              act r i
          | otherwise =
              return ActNoop
          where
            tts = tp ^. theImgTimeStamp
            sts = sp ^. theImgTimeStamp

        match st ip
          | (ip ^. theImgType == st) = (ip, emptyName)
          | otherwise                = (ip, ext)
          where
            ext = filePathToExt st (ip ^. theImgName . name2string)

        toTP tn =
          case targetPart tn of
            [] ->
              mkImgPart tn (snd target)
            (p : _) ->
              p
          where
            targetPart n' = filter (\ p -> p ^. theImgName == n') pts

        toTN part ext = part ^. theImgName . to (substNameSuffix ext (fst target))

mkCopyRule :: Int -> Int -> AspectRatio -> Rule
mkCopyRule w h ar = (rl, act)
  where
    copy'sx = mkName $ "." ++ show w ++ "x" ++ show h ++ ".jpg"

    rl = PT (copy'sx, IMGcopy) IMGjpg

    act (DP tp sp) i = do
      return $
        GenCopy i (tp ^. theImgName) (sp ^. theImgName) ar w h

mkMetaRule :: ImgType -> Rule
mkMetaRule t = (rl, act)
  where
    rl =
      PT (mkName ".json", IMGjson) t

    act (DP tp sp) i =
      return $
      GenMeta i (tp ^. theImgName) (sp ^. theImgName) t


buildRules :: Cmd [Rule]
buildRules = do
  cg <- view envCopyGeo
  cm <- view envMetaSrc
  return $
    map (uncurry . uncurry $ mkCopyRule) cg
    ++
    map mkMetaRule cm

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
