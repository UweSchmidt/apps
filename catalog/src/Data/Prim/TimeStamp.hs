{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.TimeStamp
       ( TimeStamp
       , now
       , fsTimeStamp
       )
where

import           Control.Monad (mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as J
import           Data.Prim.Prelude
import           System.Posix (FileStatus)
import qualified System.Posix as X

-- ----------------------------------------

newtype TimeStamp = TS X.EpochTime

deriving instance Eq   TimeStamp
deriving instance Ord  TimeStamp
deriving instance Show TimeStamp

instance IsoString TimeStamp where
  isoString = iso (\ (TS t) -> show t) (TS . read)
  {-# INLINE isoString #-}

instance Monoid TimeStamp where
  mempty = zeroTimeStamp
  ts1 `mappend` ts2 = ts1 `max` ts2
  {-# INLINE mappend #-}
  {-# INLINE mempty #-}

instance IsEmpty TimeStamp where
  isempty = (== zeroTimeStamp)
  {-# INLINE isempty #-}

instance ToJSON TimeStamp where
  toJSON = toJSON . view isoString
  {-# INLINE toJSON #-}

instance FromJSON TimeStamp where
  parseJSON (J.String t) =
    return (t ^. isoString . from isoString)
  parseJSON _ =
    mzero

zeroTimeStamp :: TimeStamp
zeroTimeStamp = TS $ read "0"
{-# INLINE zeroTimeStamp #-}

now :: MonadIO m => m TimeStamp
now = liftIO (TS <$> X.epochTime)

fsTimeStamp :: FileStatus -> TimeStamp
fsTimeStamp = TS . X.modificationTime
{-# INLINE fsTimeStamp #-}

-- ----------------------------------------
