{-# LANGUAGE OverloadedStrings #-}

module Catalog.System.CatalogIO
where

import           Catalog.Cmd.Basic
import           Catalog.Cmd.Invariant    (checkImgStore)
import           Catalog.Cmd.Types
import           Catalog.Journal
import           Catalog.System.IO
import qualified Data.Aeson               as J
import qualified Data.Aeson.Encode.Pretty as J
import           Data.Prim
import qualified System.FilePath          as FP

-- ----------------------------------------

encodeJSON :: ToJSON a => a -> LazyByteString
encodeJSON = J.encodePretty' conf
  where
    conf = J.defConfig
           { J.confIndent  = J.Spaces 2 }

-- save the whole image store
-- file path must be a relative path
-- to the mount path

saveImgStore :: FilePath -> Cmd ()
saveImgStore p = do
  bs <- toBS
  sp <- toSysPath p
  verbose $ "saveImgStore: save state to " ++ show sp
  writeFileLB sp bs
  journalChange $ SaveImgStore p
  where
    toBS
      | isHashIdArchive p =
          encodeJSON <$> get
      | isPathIdArchive p =
          encodeJSON <$> mapImgStore2Path
      | otherwise =
          abort $ "saveImgStore: wrong archive extenstion in " ++ show p

-- take a snapshot of the catalog and store it in a git archive
-- Test: save 2 versions of the catalog
-- with ObjId (hashes) as keys and (more readable) with Path as keys
--
-- both can be loaded during server startup

snapshotImgStore :: String -> Cmd ()
snapshotImgStore cmt = do
  pt <- view envJsonArchive

  verbose $ "snapshotImgStore: make a snapshot into " ++ show pt
  saveImgStore pt
  checkinImgStore cmt pt

  whenM (view envSaveBothIx) $ do
    let pt' = switchArchiveName pt
    verbose $ "snapshotImgStore: make a snapshot into " ++ show pt'
    saveImgStore pt'
    checkinImgStore cmt pt'

checkinImgStore :: String -> FilePath -> Cmd ()
checkinImgStore cmt f = do
  pt <- toSysPath f
  ts <- nowAsIso8601
  verbose $ unwords ["bash  []", checkinScript pt ts]
  void $ execProcess "bash" [] $ checkinScript pt ts
  where
    qt s = '\'' : s ++ "'"

    cmt' | null cmt  = cmt
         | otherwise = ", " ++ cmt

    -- the git script is somewhat fragile,
    -- if there are untracked files in the dir
    -- git returns exit code 1, this generates an error
    -- even if everything was o.k.
    checkinScript :: SysPath -> String -> String
    checkinScript pt ts =
      unwords
      [ "cd", qt $ FP.takeDirectory fp, ";"
      , "git add -A ;"
      , "git diff --quiet --exit-code --cached ||"
      , "git commit -a -m", qt ("catalog-server: " ++ ts ++ ", " ++ fp ++ cmt')
      ]
      where
        fp   = pt ^. isoFilePath


loadImgStore :: FilePath -> Cmd ()
loadImgStore f = do
  sp <- toSysPath f
  verbose $ "loadImgStore: load State from " ++ show (sp ^. isoFilePath)
  bs <- readFileLB sp
  case fromBS bs of
    Nothing ->
      abort $ "loadImgStore: JSON input corrupted: " ++ show (sp ^. isoFilePath)
    Just st -> do
      put st
      journalChange $ LoadImgStore f
      -- make a "FS check" and throw away undefined refs
      checkImgStore
  where
    fromBS
      | isPathIdArchive f = fmap mapImgStore2ObjId . J.decode'
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
