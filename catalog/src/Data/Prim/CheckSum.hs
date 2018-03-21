{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Data.Prim.CheckSum
       ( CheckSum
       , mkCheckSum
       , mkFileCheckSum
       )
where

-- import           Data.Bits
import qualified Data.Digest.Murmur64 as MM
-- import           Data.Foldable
import           Data.Word (Word64)
import           Data.Prim.Prelude

-- ----------------------------------------

newtype CheckSum  = CS Word64

zeroCheckSum :: CheckSum
zeroCheckSum = CS 0

mkCheckSum :: MM.Hashable64 a => a -> CheckSum
mkCheckSum = CS . MM.asWord64 . MM.hash64

fromCheckSum :: Integral a => CheckSum -> a
fromCheckSum (CS csum) = fromIntegral csum

toCheckSum :: Integer -> CheckSum
toCheckSum = CS . fromInteger

deriving instance Eq CheckSum

instance IsEmpty CheckSum where
  isempty = (== zeroCheckSum)

instance Semigroup CheckSum where
  c1 <> c2
    | isempty c1 = c2
    | otherwise  = c1

instance Monoid CheckSum where
  mempty  = zeroCheckSum
  mappend = (<>)

instance IsoInteger CheckSum where
  isoInteger = iso fromCheckSum toCheckSum

instance IsoString CheckSum where
  isoString = iso showCheckSum readCheckSum

instance Show CheckSum where
  show = ("0x" ++) . showCheckSum

showCheckSum :: CheckSum -> String
showCheckSum (CS w) =
  i ^. isoHex
  where
    i :: Int
    i = fromIntegral w

readCheckSum :: String -> CheckSum
readCheckSum s =
  CS $ fromIntegral i
  where
    i :: Int
    i = s ^. from isoHex

instance IsoHex CheckSum where
  isoHex = iso showCheckSum readCheckSum

instance ToJSON CheckSum where
  toJSON = toJSON . showCheckSum

instance FromJSON CheckSum where
  parseJSON o = readCheckSum <$> parseJSON o

-- ----------------------------------------

-- | compute the checksum for a simple file
-- does not work for directories

mkFileCheckSum :: FilePath -> IO CheckSum
mkFileCheckSum p = mkCheckSum <$> readFile p

-- ----------------------------------------
