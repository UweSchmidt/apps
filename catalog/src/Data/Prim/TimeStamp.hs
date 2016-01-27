{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.TimeStamp
where

import           Control.Lens hiding (children)
import           Control.Monad (mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as J
import           Data.Aeson (ToJSON, FromJSON, toJSON)
import qualified Data.Text as T
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

instance ToJSON TimeStamp where
  toJSON = toJSON . view timeStamp2string

instance FromJSON TimeStamp where
  parseJSON (J.String t) = return ((view $ from timeStamp2string) . T.unpack $ t)
  parseJSON _            = mzero

now :: MonadIO m => m TimeStamp
now = liftIO (TS <$> X.epochTime)

fsTimeStamp :: FileStatus -> TimeStamp
fsTimeStamp = TS . X.modificationTime

-- ----------------------------------------
