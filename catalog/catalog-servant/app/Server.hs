{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
-- import Control.Monad.Reader
-- import Data.Aeson.Compat
-- import Data.Aeson.Types
-- import Data.Attoparsec.ByteString
-- import Data.ByteString (ByteString)
-- import Data.List
-- import Data.Maybe
-- import Data.String.Conversions
-- import Data.Time.Calendar
-- import GHC.Generics
-- import Lucid
-- import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger       (withStdoutLogger)
import Servant
-- import System.Directory
-- import Text.Blaze
-- import Text.Blaze.Html.Renderer.Utf8
-- import qualified Data.Aeson.Parser
-- import qualified Text.Blaze.Html

import Control.Concurrent.QSem
import Control.Exception.Base (bracket_)
import Control.Exception (SomeException, catch) -- , try, toException)
import Control.Concurrent.MVar
import qualified
       Control.Monad.ReaderStateErrIO as RSE
import System.Exit (die)
import System.FilePath (FilePath)
import System.IO (hPutStrLn, stderr, hFlush)

import Data.Prim
import Data.ImgNode
import Data.ImgTree
import Data.MetaData
import Data.ImageStore (ImgStore)

import Catalog.Cmd
{-
(Env, Cmd
                   , runAction
                   , initState
                   , envPort
--                   , envVerbose
--                   , envMountPath
                   , envLogOp
                   )
-}
import Catalog.Options (mainWithArgs)
import Catalog.Html.Basic ( buildImgPath
                          , colImgRef
                          , getColBlogSource
                          , putColBlogSource
                          , getColBlogCont
                          )
import Catalog.System.ExifTool (getMetaData, forceSyncAllMetaData)

-- import Web.HttpApiData (parseUrlPieceWithPrefix)
-- import qualified Data.Text as T

import API

-- ----------------------------------------
--
-- conversions of [Text] to Path

ttp :: (Path -> a) -> ([Text] -> a)
ttp f xs = f (listToPath xs)

ttp2 :: (a -> Path -> b) -> (a -> [Text] -> b)
ttp2 = flip . ttp . flip


-- ----------------------------------------
--
-- adapter for catalog commands to handler

pin :: ((ObjId, ImgNode) -> a) ->
       (a -> Cmd r) ->
       (Path -> Cmd r)
pin sel cmd path = do
  v <- lookupByPath path
  case v of
    Nothing ->
      abort $ "entry not found: " <> path ^. isoString
    Just i'n ->
      cmd $ sel i'n

mkcmd0 :: (Cmd r -> Handler r) ->
          (Path -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd0 toHandler pcmd =
  toHandler . pcmd . listToPath

mkcmd1i :: (Cmd r -> Handler r) ->
           (ObjId -> Cmd r) ->
           ([Text] -> Handler r)
mkcmd1i toHandler pcmd =
  toHandler . pin fst pcmd . listToPath

mkcmd1n :: (Cmd r -> Handler r) ->
           (ImgNode -> Cmd r) ->
           ([Text] -> Handler r)
mkcmd1n toHandler pcmd =
  toHandler . pin snd pcmd . listToPath

mkcmd2i :: (Cmd r -> Handler r) ->
           (a -> ObjId -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2i toHandler pcmd path args =
  toHandler . (pin fst $ pcmd args) . listToPath $ path

mkcmd2n :: (Cmd r -> Handler r) ->
           (a -> ImgNode -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2n toHandler pcmd path args =
  toHandler . (pin snd $ pcmd args) . listToPath $ path

-- ----------------------------------------
-- the server

catalogServer :: FilePath ->
                 (forall a . Cmd a -> Handler a) ->
                 (forall a . Cmd a -> Handler a) ->
                 Server CatalogAPI
catalogServer mp runR runM =
  ( bootstrap
    :<|>
    ( assets'css
      :<|>
      assets'icons
      :<|>
      assets'javascript
    )
    :<|>
    edit
  )
  :<|>
  ( ttp2 blaze
    :<|>
    imgcopy
  )
  :<|>
  ( json'read
    :<|>
    json'modify
  )
  where
    static p = serveDirectoryWebApp (mp ++ p)

    bootstrap         = static "/bootstrap"
    assets'css        = static ps'css
    assets'icons      = static ps'icons
    assets'javascript = static ps'javascript
    edit              = static "/edit.html"

    blaze :: BlazeHTML -> Path -> Handler String
    blaze (BlazeHTML (Geo' geo)) path
      | checkExtPath ".html" path = return $ show (geo, path)
      | otherwise                 = throwError err404

    imgcopy (GeoAR' geo) =
      imgcopy'archive geo
      :<|>
      imgcopy'others  geo
      where
        imgcopy'archive = ttp2 imgcopy'
        imgcopy'others  = ttp2 imgcopy'

    imgcopy' :: GeoAR -> Path -> Handler String
    imgcopy' geo path
      | checkExtPath ".jpg" path  = return $ show (geo, path)
      | otherwise                 = throwError err404

    mkR0  = mkcmd0  runR
    mkR1i = mkcmd1i runR
    mkR1n = mkcmd1n runR
    mkR2i = mkcmd2i runR
    mkR2n = mkcmd2n runR

    json'read =
      mkR1n read'collection
      :<|>
      mkR1n read'isWriteable
      :<|>
      mkR1n read'isRemovable
      :<|>
      mkR1n read'isSortable
      :<|>
      mkR0  read'isCollection
      :<|>
      mkR2i read'iconref
      :<|>
      mkR2n read'blogcontents
      :<|>
      mkR2n read'blogsource
      :<|>
      mkR2n read'previewref
      :<|>
      mkR2n read'metadata
      :<|>
      mkR2n read'rating
      :<|>
      mkR1n read'ratings
      where

    mkM1 = mkcmd1n runM

    json'modify =
      ttp modify'syncCol
      :<|>
      ttp modify'saveblogsource
      where
        modify'syncCol :: Path -> Handler Path
        modify'syncCol path = return path

        modify'saveblogsource :: Path -> (Int, Text) -> Handler (Path, Int, Text)
        modify'saveblogsource path (pos, val) = return (path, pos, val)

-- ----------------------------------------
--
-- process command line args,
-- build env, configure log command
-- and init server state

main :: IO ()
main = mainWithArgs "servant" $ \ env -> do
  -- create a semaphore for syncing log output
  sem  <- newQSem 0
  let env' = env & envLogOp .~ logCmd sem
                 & envMountPath .~ "/Users/uwe/haskell/apps/catalog/data"
                 & envJsonArchive .~ "catalog/photos.hashid.json"
                 & envPort .~ 8081

  est  <- initState env'
  either die (main' env') est
  where
    logCmd :: QSem -> (String -> IO ())
    logCmd sem s =
      bracket_ (waitQSem sem) (signalQSem sem)
      ( do hPutStrLn stderr s
           hFlush    stderr
      )

main' :: Env -> ImgStore -> IO ()
main' env st = do
  -- create MVars for the image archive state
  mvRead <- newMVar st
  mvMody <- newMVar st

  let runRead  = runReadCmd env mvRead
  let runMody  = runModyCmd env mvRead mvMody

  withStdoutLogger $ \logger -> do
    let settings = setPort (env ^. envPort) $ setLogger logger defaultSettings
    runSettings settings $
      serve (Proxy :: Proxy CatalogAPI) $
      catalogServer (env ^. envMountPath) runRead runMody

-- curl -v http://localhost:8081/bootstrap/dist/css/bootstrap-theme.css
-- curl -v http://localhost:8081/assets/javascript/html-album.js
-- curl -v http://localhost:8081/edit.html
-- curl -v http://localhost:8081/blaze-1920x1200/archive/photos.html
-- curl -v http://localhost:8081/pad-1920x1200/archive/photos.jpg
-- curl -v http://localhost:8081/pad-1920x1200/cache/assets/icons/photos.jpg

-- ----------------------------------------

-- there are 2 mvars for the image store
--
-- one for reading operations, those can run in parallel
-- so the mvar is not emptied when starting an action
--
-- the 2. is for modifying the store, those operations run sequentially
-- and at the end they update both mvars wit the new state

runReadCmd :: Env -> MVar ImgStore -> Cmd a -> Handler a
runReadCmd env mvs cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- readMVar mvs
      res <-
        ((^. _1) <$> runAction cmd env store)
        `catch`
        -- TODO this still does not catch: error "some error"
        (\ e -> return (Left . RSE.Msg . show $ (e :: SomeException)))
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> Handler a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- takeMVar mvm
      (res, new'store) <- runAction cmd env store

      -- TODO try to catch: error "some error" like in runReadCmd

      _old <- swapMVar mvr new'store
      putMVar mvm new'store
      return res

raise500 :: RSE.Msg -> Handler a
raise500 (RSE.Msg msg) =
  throwError $ err500 { errBody = msg' }
  where
    msg' = show msg ^. from isoString

-- ----------------------------------------
--
-- command for quering the catalog

read'collection :: ImgNode -> Cmd ImgNodeP
read'collection n = mapObjId2Path n

read'isWriteable :: ImgNode -> Cmd Bool
read'isWriteable n = return (isWriteable  $ n ^. theColMetaData)

read'isRemovable :: ImgNode -> Cmd Bool
read'isRemovable n = return (isRemovable  $ n ^. theColMetaData)

read'isSortable :: ImgNode -> Cmd Bool
read'isSortable n = return (isSortable  $ n ^. theColMetaData)

read'isCollection :: Path -> Cmd Bool
read'isCollection p = do
  v <- lookupByPath p
  case v of
    Nothing ->
      return False
    Just (_i, n) ->
      return $ isCOL n

read'iconref :: GeoAR -> ObjId -> Cmd FilePath
read'iconref geo i =
  (("/" ++ geo ^. isoString) ++) <$> colImgRef i

read'blogcontents :: Int -> ImgNode -> Cmd Text
read'blogcontents =
  processColEntryAt
    getColBlogCont                        -- ImgRef: entry is a blog text
    (\ i    -> do                         -- ColRef: lookup the col blog ref
        be <- getImgVals i theColBlog
        maybe (return mempty)             -- return nothing, when not there
              (uncurry getColBlogCont)    -- else generate the HTML
              be
    )

read'blogsource :: Int -> ImgNode -> Cmd Text
read'blogsource =
  processColEntryAt
    getColBlogSource
    (\ i    -> do
        be        <- getImgVals i theColBlog
        (bi, bn)  <- maybe
                     ( do p <- objid2path i
                          abort ("getBlogCont: no blog entry set in collection: "
                                 ++ p ^. isoString)
                     )
                     return
                     be
        getColBlogSource bi bn
    )

read'previewref :: (Int, GeoAR) -> ImgNode -> Cmd FilePath
read'previewref (pos, geo) n =
  (("/" ++ geo ^. isoString) ++) <$>
  processColEntryAt
    buildImgPath
    colImgRef
    pos n

read'metadata :: Int -> ImgNode -> Cmd MetaData
read'metadata =
  processColEntryAt
    (\ i _ -> do exifMD <- getMetaData i               -- exif meta data
                 imgMD  <- getImgVals  i theMetaData   -- title, comment, ...
                 return $ imgMD <> exifMD
    )
    (\ i   -> getImgVals i theMetaData)

read'rating :: Int -> ImgNode -> Cmd Rating
read'rating pos n =
  getRating <$>
  processColEntryAt (\ i' _ -> getM i') getM pos n
  where
    getM i' = getImgVals i' theMetaData

read'ratings :: ImgNode -> Cmd [Rating]
read'ratings n =
  traverse f (n ^. theColEntries)
  where
    f = colEntry (\ i' _ -> getR i') getR
      where
        getR i' = getRating <$> getImgVals i' theMetaData

-- ----------------------------------------