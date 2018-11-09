{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.TimeStamp
       ( TimeStamp
       , now
       , fsTimeStamp
       , isoEpochTime
       )
where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as J
import           Data.Prim.Prelude
import           System.Posix (FileStatus)
import qualified System.Posix as X

-- ----------------------------------------

newtype TimeStamp = TS X.EpochTime

isoEpochTime :: Iso' TimeStamp X.EpochTime
isoEpochTime = iso (\ (TS et) -> et) TS

deriving instance Eq   TimeStamp
deriving instance Ord  TimeStamp
deriving instance Show TimeStamp

instance IsoString TimeStamp where
  isoString = iso
              (\ (TS t) -> show t)
              (\ s -> fromMaybe zeroTimeStamp $ TS <$> readMaybe s)
  {-# INLINE isoString #-}

instance IsoText TimeStamp where
  isoText = isoString . isoText

instance Semigroup TimeStamp where
  (<>) = max

instance Monoid TimeStamp where
  mempty  = zeroTimeStamp
  mappend = (<>)

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
zeroTimeStamp = TS 0
{-# INLINE zeroTimeStamp #-}

now :: MonadIO m => m TimeStamp
now = liftIO (TS <$> X.epochTime)

fsTimeStamp :: FileStatus -> TimeStamp
fsTimeStamp = TS . X.modificationTime
{-# INLINE fsTimeStamp #-}

-- ----------------------------------------
