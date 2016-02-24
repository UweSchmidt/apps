{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.MetaData
where

import           Control.Lens
import           Control.Monad.Except
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as HM
import           Data.Prim.Name
import           Data.Prim.Prelude
import qualified Data.Text as T
import qualified Data.Vector as V

-- ----------------------------------------

newtype MetaData = MD J.Object

-- ----------------------------------------

deriving instance Show MetaData

instance Monoid MetaData where
  mempty                = MD HM.empty
  MD m1 `mappend` MD m2 = MD $ m1 `HM.union` m2

instance ToJSON MetaData where
  toJSON (MD m) = J.toJSON [m]

instance FromJSON MetaData where
  parseJSON = J.withArray "MetaData" $ \ v ->
    case V.length v of
      1 -> J.withObject "MetaData" (return . MD) (V.head v)
      _ -> mzero

metaDataAt :: Name -> Lens' MetaData Text
metaDataAt key = md2obj . at (key ^. name2text) . val2text
  where
    md2obj :: Iso' MetaData J.Object
    md2obj = iso (\ (MD m) -> m) MD

    val2text :: Iso' (Maybe J.Value) Text
    val2text = iso totext fromtext
      where
        totext (Just (J.String t)) = t
        totext _                   = ""

        fromtext t
          | T.null t = Nothing
          | otherwise = Just (J.String t)


partMetaData :: (Name -> Bool) -> Iso' MetaData (MetaData, MetaData)
partMetaData predicate = iso part (uncurry mappend)
  where
    part (MD m) = (MD *** MD) $ HM.foldrWithKey pf (HM.empty, HM.empty) m
      where
        pf k v (m1, m2)
          | predicate (k ^. from name2text) =
              (HM.insert k v m1, m2)
          | otherwise =
              (m1, HM.insert k v m2)

partByRegex :: Text -> Iso' MetaData (MetaData, MetaData)
partByRegex rx = partMetaData p
  where
    rx' = parseRegexExt rx
    p n = matchRE rx' (n ^. name2text)

-- ----------------------------------------
