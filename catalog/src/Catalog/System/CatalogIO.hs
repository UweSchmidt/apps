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
  bs <- toBS
  p' <- (</> p) <$> view envMountPath
  verbose $ "saveImgStore: save state to " ++ show p'
  writeFileLB p' bs
  journalChange $ SaveImgStore p
  where
    toBS
      | isHashIdArchive p =
          J.encodePretty <$> get
      | isPathIdArchive p =
          J.encodePretty <$> mapImgStore2Path
      | otherwise =
          abort $ "saveImgStore: wrong archive extenstion in " ++ show p

-- take a snapshot of the catalog and store it in a git archive
-- Test: save 2 versions of the catalog
-- with ObjId (hashes) as keys and (more readable) with Path as keys
--
-- both can be loaded during server startup

snapshotImgStore :: Cmd ()
snapshotImgStore = do
  pt <- view envJsonArchive

  verbose $ "snapshotImgStore: make a snapshot into " ++ show pt
  saveImgStore pt
  checkinImgStore pt

  let pt' = switchArchiveName pt
  verbose $ "snapshotImgStore: make a snapshot into " ++ show pt'
  saveImgStore pt'
  checkinImgStore pt'

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
      , "git commit -a -m", qt ("catalog-server: " ++ ts)
      ]
    qt s = '\'' : s ++ "'"

loadImgStore :: FilePath -> Cmd ()
loadImgStore p = do
  p' <- (</> p) <$> view envMountPath
  verbose $ "loadImgStore: load State from " ++ show p'
  bs <- readFileLB p'
  case fromBS bs of
    Nothing ->
      abort $ "loadImgStore: JSON input corrupted: " ++ show p
    Just st -> do
      put st
      journalChange $ LoadImgStore p
  where
    fromBS
      | isPathIdArchive p = fmap mapImgStore2ObjId . J.decode'
      | otherwise         = J.decode'

archiveName :: FilePath -> (FilePath, String, String)
archiveName p = (p', e1, e2)
  where
    (p1, e2) = FP.splitExtension p
    (p', e1) = FP.splitExtension p1

isHashIdArchive :: FilePath -> Bool
isHashIdArchive p = e1 == hidx
  where
    (_, e1, _) = archiveName p

isPathIdArchive :: FilePath -> Bool
isPathIdArchive p = e1 == pidx
  where
    (_, e1, _) = archiveName p

switchArchiveName :: FilePath -> FilePath
switchArchiveName p =
  p' ++ e1' ++ e2'
  where
    (p', e1, e2') = archiveName p
    e1' | e1 == hidx = pidx
        | e1 == pidx = hidx
        | otherwise  = e1

pidx, hidx :: String
pidx = ".pathid"
hidx = ".hashid"

-- ----------------------------------------
