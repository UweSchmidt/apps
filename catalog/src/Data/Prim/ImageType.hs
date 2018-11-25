{-# LANGUAGE StandaloneDeriving #-}

module Data.Prim.ImageType
where

import           Data.Prim.Name
import           Data.Prim.Prelude

-- ----------------------------------------

type NameImgType = (Name, ImgType)

data ImgType =
  IMGraw   | IMGmeta   | IMGjson   | IMGjpg    |
  IMGimg   | IMGcopy   | IMGimgdir | IMGjpgdir |
  IMGother | IMGboring | IMGhugin  | IMGdxo    |
  IMGtxt   | IMGdng    | IMGmovie

deriving instance Eq   ImgType
deriving instance Ord  ImgType
deriving instance Show ImgType
deriving instance Read ImgType

instance ToJSON ImgType where
  toJSON = toJSON . show
  {-# INLINE toJSON #-}

instance FromJSON ImgType where
  parseJSON o = read <$> parseJSON o

instance Semigroup ImgType where
  IMGother <> t2 = t2
  t1       <> _  = t1

instance Monoid ImgType where
  mempty  = IMGother
  mappend = (<>)

instance IsEmpty ImgType where
  isempty = (== IMGother)

-- is .jpg file
isJpg :: ImgType -> Bool
isJpg IMGjpg = True
isJpg _      = False

-- is .txt or .md file
isTxt :: ImgType -> Bool
isTxt IMGtxt = True
isTxt _      = False

-- is sub dir with images developped as .jpg
isImgSubDir :: ImgType -> Bool
isImgSubDir IMGimgdir = True
isImgSubDir _         = False

-- file for extracting metadata
isRawMeta :: ImgType -> Bool
isRawMeta ty = ty `elem` [IMGraw, IMGimg, IMGmeta, IMGmovie]

-- file, that is stored as part of an image entry
isAnImgPart :: ImgType -> Bool
isAnImgPart ty = ty `elem` [ IMGraw, IMGmeta, IMGjson
                           , IMGjpg, IMGimg,  IMGcopy
                           , IMGtxt, IMGmovie
                           ]

-- part of an image entry which can be shown in a collection
-- a subset of isAnImgPart

isShowablePart :: ImgType -> Bool
isShowablePart ty = ty `elem` [IMGjpg, IMGimg, IMGtxt, IMGmovie]

-- is image with camera EXIF info
isImgWithMeta :: ImgType -> Bool
isImgWithMeta ty = ty `elem` [IMGraw, IMGjpg, IMGimg, IMGmovie]

-- is .xmp file with meta data from Lightroom (e.g. GPS or rating)
isMeta :: ImgType -> Bool
isMeta IMGmeta = True
isMeta _       = False


-- files which are ignored when syncing with filesystem
isBoring :: ImgType -> Bool
isBoring IMGboring = True
isBoring _         = False

-- ----------------------------------------
