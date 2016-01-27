{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Data.Prim.CheckSum
where

import           Control.Lens (Iso', iso)
import           Data.Aeson (ToJSON, FromJSON(..), toJSON)
import           Data.Bits
import qualified Data.Digest.Murmur64 as MM
import           Data.Foldable
import           Data.Word (Word64)

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

checkSum2integer :: Iso' CheckSum Integer
checkSum2integer = iso fromCheckSum toCheckSum

deriving instance Eq CheckSum

instance Show CheckSum where
  show = ("0x" ++) . showCheckSum

showCheckSum :: CheckSum -> String
showCheckSum (CS csum) =
  toHex 16 csum []
  where
    toHex :: Int -> Word64 -> String -> String
    toHex  0  _  acc = acc
    toHex !n !w !acc = let !c = toDig (w .&. 0xF)
                       in
                         toHex (n - 1) (w `shiftR` 4) (c : acc)

    toDig :: Word64 -> Char
    toDig w
      | w < 10    = toEnum $ fromEnum w + fromEnum '0'
      | otherwise = toEnum $ fromEnum w + (fromEnum 'a' - 10)

readCheckSum :: String -> CheckSum
readCheckSum = CS . foldl' nextDig 0
  where
    nextDig !acc !c
      | '0' <= c && c <= '9' = (acc `shiftL` 4) .|. toEnum (fromEnum c - fromEnum '0')
      | otherwise            = (acc `shiftL` 4) .|. toEnum (fromEnum c - fromEnum 'a' + 10)

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
