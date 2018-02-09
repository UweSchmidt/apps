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
-- import Network.Wai
import Network.Wai.Handler.Warp
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
-- import Data.ImgNode
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


pin :: (Path -> ObjId -> ImgNode -> Cmd r) ->
       (Path -> Cmd r)
pin cmd path = do
  v <- lookupByPath path
  case v of
    Nothing ->
      abort $ "entry not found: " <> path ^. isoString
    Just (i, n) ->
      cmd path i n

mkcmd1 :: (Cmd r -> Handler r) ->
          (Path -> ObjId -> ImgNode -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd1 toHandler pcmd =
  toHandler . pin pcmd . listToPath

mkcmd2 :: (Cmd r -> Handler r) ->
          (a -> Path -> ObjId -> ImgNode -> Cmd r) ->
          ([Text] -> a -> Handler r)
mkcmd2 toHandler pcmd path args =
  toHandler . (pin $ pcmd args) . listToPath $ path

type PathCmd r = Path -> ObjId -> ImgNode -> Cmd r

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

    mkR1 = mkcmd1 runR
    mkR2 = mkcmd2 runR

    json'read =
      mkR1 read'isSortable
      :<|>
      ttp read'isCollection
      :<|>
      mkR2 read'iconref
      :<|>
      mkR2 read'blogcontents
      :<|>
      mkR2 read'blogsource
      :<|>
      mkR2 read'previewref
      :<|>
      mkR2 read'metadata
      :<|>
      mkR2 read'rating
      :<|>
      mkR1 read'ratings
      where
        read'isSortable :: PathCmd Bool
        read'isSortable _p _i n = return (isSortable  $ n ^. theColMetaData)

        read'isCollection :: Path -> Handler Bool
        read'isCollection _path = return True

        read'iconref :: GeoAR -> PathCmd (GeoAR, Path)
        read'iconref geo p _i _n = return (geo, p)

        read'blogcontents :: Int -> PathCmd (Int, Path)
        read'blogcontents pos p _i _n = return (pos, p)

        read'blogsource :: Int -> PathCmd (Int, Path)
        read'blogsource pos p _i _n = return (pos, p)

        read'previewref :: (Int, GeoAR) -> PathCmd (Path, Int, GeoAR)
        read'previewref (pos, geo) p _i _n = return (p, pos, geo)

        read'metadata :: Int -> PathCmd (Path, Int)
        read'metadata pos p _i _n = return (p, pos)

        read'rating :: Int -> PathCmd (Path, Int)
        read'rating pos p _i _n = return (p, pos)

        read'ratings :: PathCmd Path
        read'ratings p _i _n = return p

    mkM1 = mkcmd1 runM

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

  run (env ^. envPort) $
    serve (Proxy :: Proxy CatalogAPI) $
    catalogServer mountPoint runRead runMody
  where
    mountPoint :: String
    mountPoint = "/Users/uwe/haskell/apps/catalog/data"
--  mountPoint = env ^. envMountPath


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
raise500 msg =
  throwError $ err500 { errBody = msg' }
  where
    msg' = show msg ^. from isoString

-- ----------------------------------------
