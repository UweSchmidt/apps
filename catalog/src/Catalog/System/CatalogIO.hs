{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.CatalogIO
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Types
import           Catalog.Journal
import           Catalog.System.IO
import           Catalog.FilePath
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Data.Prim
import qualified System.FilePath as FP

-- ----------------------------------------

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  bs <- uses id J.encodePretty
  if null p
    then putStrLnLB    bs
    else do
      p' <- (</> p) <$> view envMountPath
      verbose $ "saveImgStore: save state to " ++ show p'
      writeFileLB p' bs
      journalChange $ SaveImgStore p


snapshotImgStore :: Cmd ()
snapshotImgStore = do
  pt <- view envJsonArchive
  verbose $ "snapshotImgStore: make a snapshot into " ++ show pt
  saveImgStore pt
  checkinImgStore pt

checkinImgStore :: FilePath -> Cmd ()
checkinImgStore pt = do
  ts <- nowAsIso8601
  verbose $ unwords ["bash []", checkinScript ts]
  void $ execProcess "bash" [] $ checkinScript ts
  where
    -- the git script is somewhat fragile
    -- if there are untracked files in the dir
    -- git returns exit code 1, this generates an error
    -- even if everything was o.k.
    checkinScript ts =
      unwords
      [ "cd", qt $ FP.takeDirectory pt, ";"
      , "git add -A ;"
      , "git diff --quiet --exit-code --cached ||"
      , "git commit -m", qt ("catalog-server: " ++ ts), qt $ FP.takeFileName pt
      ]
    qt s = '\'' : s ++ "'"

loadImgStore :: FilePath -> Cmd ()
loadImgStore p = do
  p' <- (</> p) <$> view envMountPath
  verbose $ "loadImgStore: load State from " ++ show p'
  bs <- readFileLB p'
  case J.decode' bs of
    Nothing ->
      abort $ "loadImgStore: JSON input corrupted: " ++ show p
    Just st -> do
      put st
      journalChange $ LoadImgStore p

-- ----------------------------------------
