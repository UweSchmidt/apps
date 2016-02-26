{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.TimeStamp
where

import           Control.Lens
import           Control.Lens.Util
import           Control.Monad (mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as J
import           Data.Prim.Prelude
import           System.Posix (FileStatus)
import qualified System.Posix as X

-- ----------------------------------------

newtype TimeStamp = TS X.EpochTime

zeroTimeStamp :: TimeStamp
zeroTimeStamp = TS $ read "0"

timeStamp2string :: Iso' TimeStamp String
timeStamp2string
  = iso (\ (TS t) -> show t) (TS . read)

deriving instance Eq   TimeStamp
deriving instance Ord  TimeStamp
deriving instance Show TimeStamp

instance Monoid TimeStamp where
  mempty = zeroTimeStamp
  ts1 `mappend` ts2 = ts1 `max` ts2

instance ToJSON TimeStamp where
  toJSON = toJSON . view timeStamp2string

instance FromJSON TimeStamp where
  parseJSON (J.String t) =
    return (t ^. from isoStringText . from timeStamp2string)
  parseJSON _ =
    mzero

now :: MonadIO m => m TimeStamp
now = liftIO (TS <$> X.epochTime)

fsTimeStamp :: FileStatus -> TimeStamp
fsTimeStamp = TS . X.modificationTime

-- ----------------------------------------
