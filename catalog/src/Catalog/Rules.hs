{-# LANGUAGE StandaloneDeriving #-}

module Catalog.Rules
where

import Catalog.Cmd
import Catalog.FilePath
import Control.Lens
import Data.ImageTree
import Data.ImgAction
import Data.Prim

-- ----------------------------------------

data Pattern = PT NameImgType ImgType
data Deps    = DP ImgPart ImgPart
type Rule    = (Pattern, Deps -> ObjId -> Cmd ImgAction)

-- ----------------------------------------

deriving instance Show Pattern
deriving instance Show Deps

-- ----------------------------------------

applyRules :: [Rule] -> ObjId -> Cmd ImgAction
applyRules rls i0 = processImages (matchRules rls) i0

matchRules :: [Rule] -> ObjId -> ImgParts -> Cmd ImgAction
matchRules rs i ps = do
  trcObj i $ "matchRules"
  mconcat <$> mapM (matchRule (ps ^. isoImgParts)) rs
  where
    matchRule pts (PT target source, act) = do
      -- trc $ "matchRule: " ++ show (target, source, map snd sps', pts ^.. traverse . theImgName)
      mconcat <$> mapM apply dps
      where
        sps = filter (not . isempty . snd) $ sps'
        sps' =      map (match' source) pts
        dps = map (\ (p, e) -> DP (toTP $ toTN p e) p) sps

        apply :: Deps -> Cmd ImgAction
        apply r@(DP tp sp)
          | isempty tts
            ||
            tts < sts =
              act r i
          | otherwise =
              return ActNoop
          where
            tts = tp ^. theImgTimeStamp
            sts = sp ^. theImgTimeStamp

        match' st ip
          | (ip ^. theImgType /= st) = (ip, mempty)
          | otherwise                = (ip, ext)
          where
            ext = filePathToExt st (ip ^. theImgName . isoString)

        toTP tn =
          case targetPart tn of
            [] ->
              mkImgPart tn (snd target)
            (p : _) ->
              p
          where
            targetPart n' = filter (\ p -> p ^. theImgName == n') pts

        toTN part ext =
          part ^. theImgName . to (substNameSuffix ext (fst target))

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

-- ----------------------------------------
