module Catalog.RunImgAction
where

import           Catalog.Cmd
import           Catalog.System.Convert
import           Catalog.System.ExifTool
import           Data.ImgAction
import           Data.MetaData
import           Data.Prim

-- ----------------------------------------

runImgAction :: ImgAction -> Cmd ()
runImgAction ActNoop =
  return ()

runImgAction (ActSeq c1 c2) =
  runImgAction c1 >> runImgAction c2

runImgAction (GenCopy i t s geo) = catchAll $ do
  p  <- objid2path i
  verbose $ "generate image copy ("
            ++ geo ^. isoString
            ++ ") for image " ++ show (show p)
  tp <- toFilePath (substPathName t p)
  sp <- toFilePath (substPathName s p)
  createImageCopy geo tp sp
  return ()

runImgAction (GenMeta i t s ty) = do
  p  <- objid2path i
  verbose $ "collect metadata for image " ++ show (show p)
  tp <- toFilePath (substPathName t p)
  sp <- toFilePath (substPathName s p)
  m1 <- readMetaData tp
  m2 <- filterMetaData ty <$> getExifTool sp

  -- new exif data wins
  writeMetaData tp (m2 <> m1)

runImgAction c = do
  trc $ "runImgAction: not implemented: " ++ show c
  return ()

-- ----------------------------------------
