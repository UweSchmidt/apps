{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Prim.SysPath
  ( SysPath
  , isoFilePath
  , emptySysPath
  , mkSysPath
  )
where

import Data.Prim.Prelude

--
-- file paths for file system paths with mount point prefix

newtype SysPath' a = SP {_unSP :: a}
  deriving (Eq, Ord, Show, Functor)

type SysPath = SysPath' FilePath

isoFilePath :: Iso' SysPath FilePath
isoFilePath = iso _unSP SP

instance ToJSON SysPath where
  toJSON = toJSON . _unSP

instance FromJSON SysPath where
  parseJSON o = SP <$> parseJSON o

instance IsEmpty SysPath where
  isempty (SP "") = True
  isempty _       = False

emptySysPath :: SysPath
emptySysPath = SP mempty

mkSysPath :: FilePath -> SysPath
mkSysPath = SP

-- ----------------------------------------
