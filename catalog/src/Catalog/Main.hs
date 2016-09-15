{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-name-shadowing #-}

module Catalog.Main
where

import           Catalog.Cmd
import           Catalog.Sync
import           Catalog.System.Convert
import           Catalog.System.ExifTool
import           Catalog.System.IO
import           Data.Prim
import           Data.ImageStore
import           Data.ImgTree
import           Data.MetaData
import           Catalog.Html.Photo2
import System.IO
import System.Exit
import qualified System.Posix as X

{-}
import           Data.ImgAction
import           Catalog.Rules
import           Catalog.RunImgAction
import           Catalog.FilePath
import qualified Data.Aeson as J
import qualified Data.Aeson.Encode.Pretty as J
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Function.Util
import           Data.List ({-intercalate,-} partition)
import           Data.Maybe
import           Data.Prim.CheckSum
import           Data.Prim.Name
import           Data.Prim.Path
import           Data.Prim.PathId
import           Data.Prim.TimeStamp
import qualified Data.Set as S
import           System.FilePath -- ((</>))
import           System.Posix (FileStatus)
import           Control.Arrow ((***))
-- -}

ccc :: IO (Either Msg (), ImgStore, Log)
ccc = runCmd $ do
  mountPath <- (</> "data") <$> getWorkingDirectory
  initImgStore n'archive n'collections (mountPath </> s'photos)
  trcCmd cwPath >> trcCmd cwLs >> return ()
  saveImgStore ""

  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)
  cwSet refImg >> trcCmd cwPath >> trcCmd cwType >> return ()

  cwe <- we
  refDir1 <- mkImgDir cwe "emil"
  cwSet refDir1 >> trcCmd cwPath >> trcCmd cwType >> trcCmd cwFilePath >> return ()

  cwe' <- we
  pic1 <- mkImg cwe' "pic1"
  pic2 <- mkImg cwe' "pic2"
  trcCmd cwLs >> return ()

  cwSet pic2 >> trcCmd cwPath >> trcCmd cwType >> trcCmd cwLs >> return ()
  cwe'' <- we
  (mkImg cwe'' "xxx" >> return ()) `catchError` (\ _ -> return ()) -- error

  cwRoot >> trcCmd cwType >> trcCmd cwLs >> trcCmd cwPath >> return ()
--  trcCmd (fromFilePath "/home/uwe/haskell/apps/catalog/emil") >> return ()
  saveImgStore ""
  rmImgNode pic1
  rmImgNode pic2
  rmImgNode refDir1


  syncFS refImg
  genCollectionRootMeta
  genCollectionsByDir
  trc "2. time gencollectionsbydir"
  genCollectionsByDir
  saveImgStore ""
  trc "save state to catalog.json"
  saveImgStore "catalog.json"
  trc "load state from catalog.json"
  loadImgStore "catalog.json"
  saveImgStore ""
  listImages >>= putStrLn'
  cwListPaths >>= putStrLn'
  cwListNames >>= putStrLn'
  -- rls <- buildRules
  -- we >>= applyRules rls >>= runImgAction

c2 :: Cmd ()
c2 = do
  loadImgStore "catalog.json"
  cwRoot
  cwSyncFS
  genCollectionsByDir
  genCollectionsByDate
  saveImgStore "catalog.json"
  saveImgStore ""
  listImages  >>= putStrLn'
  cwListPaths >>= putStrLn'
  cwListNames >>= putStrLn'
  -- rls <- buildRules
  -- we >>= applyRules rls >>= runImgAction

c3 :: Cmd a -> Cmd a
c3 c = local (envTrc .~ False) $ do
  loadImgStore "catalog.json"
  cwRoot
  local (envTrc .~ True) c
  --  saveImgStore ""
  -- saveImgStore "catalog.json"
  -- rls <- buildRules
  -- we >>= applyRules rls >>= runImgAction

syncMain :: IO ()
syncMain = do
  mountPath <- (</> "data") <$> X.getWorkingDirectory
  res <- (^. _1) <$> runCmd (local (envTrc .~ False) $ syncCatalog jsonPath mountPath)
  either
    (\ e -> do hPutStrLn stderr $ show e
               exitFailure
    )
    return
    res
  where
    jsonPath = "catalog1.json"

syncCatalog :: FilePath -> FilePath -> Cmd ()
syncCatalog jsonPath0 mountPath = do
  trc "create archive root"
  initImgStore n'archive n'collections (mountPath </> s'photos)

  let jsonPath = mountPath </> jsonPath0
  ex <- fileExist jsonPath
  when ex $ do
    trc $ "read the current archive data from file " ++ show jsonPath
    loadImgStore jsonPath

  trc "get the archive and the image root"
  refRoot <- use (theImgTree . rootRef)
  refImg  <- use (theImgTree . theNodeVal refRoot . theRootImgDir)

  trc "sync the archive with the file system"
  syncFS refImg

  trc "create the collections for the archive dirs"
  trc "and the collections per date"
  genCollectionRootMeta
  genClipboardCollection
  genCollectionsByDir
  genCollectionsByDate

  trc $ "save state to " ++ show jsonPath
  saveImgStore jsonPath

  trc "load state from json catalog and list all entries"
  loadImgStore jsonPath
  cwListNames >>= putStrLn'

runc :: Cmd a -> IO (Either Msg a, ImgStore, Log)
runc c = runCmd (c3 c)

getXXX :: Cmd Text
getXXX = genHtmlPage "/html-1600x1200/archive/collections/photos/2015/pic-0001.html"

getZZZ :: Cmd Text
getZZZ = genHtmlPage "/html-1600x1200/archive/collections/byCreateDate/2015/01/18/pic-0002.html"

getUUU :: Cmd Text
getUUU = genHtmlPage "/html-1600x1200/archive/collections/byCreateDate/2015/01.html"

getYYY :: Cmd Text
getYYY = genHtmlPage "/html-1600x1200/archive/collections/photos/2015.html"

main1 :: IO ()
main1 = do
  void $ runc $ getXXX >> return ()

main :: IO ()
main = syncMain
