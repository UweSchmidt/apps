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
  initImgStore "archive" "collections" (mountPath </> "photos")
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

main :: IO ()
main = do
  void $ runc $ getXXX >> return ()
