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

mkcmd0 :: (Cmd r -> Handler r) ->
          (Path -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd0 toHandler pcmd =
  toHandler . pcmd . listToPath

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

type ObjCmd r = Path -> ObjId -> ImgNode -> Cmd r

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

    mkR0 = mkcmd0 runR
    mkR1 = mkcmd1 runR
    mkR2 = mkcmd2 runR

    json'read =
      mkR1 read'collection
      :<|>
      mkR1 read'isWriteable
      :<|>
      mkR1 read'isRemovable
      :<|>
      mkR1 read'isSortable
      :<|>
      mkR0 read'isCollection
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
        read'collection :: ObjCmd ImgNodeP
        read'collection _p _i n = mapObjId2Path n

        read'isWriteable :: ObjCmd Bool
        read'isWriteable _p _i n = return (isWriteable  $ n ^. theColMetaData)

        read'isRemovable :: ObjCmd Bool
        read'isRemovable _p _i n = return (isRemovable  $ n ^. theColMetaData)

        read'isSortable :: ObjCmd Bool
        read'isSortable _p _i n = return (isSortable  $ n ^. theColMetaData)

        read'isCollection :: Path -> Cmd Bool
        read'isCollection p = do
          v <- lookupByPath p
          case v of
            Nothing ->
              return False
            Just (_i, n) ->
              return $ isCOL n

        read'iconref :: GeoAR -> ObjCmd FilePath
        read'iconref geo _p i _n = do
          (("/" ++ geo ^. isoString) ++) <$> colImgRef i

        read'blogcontents :: Int -> ObjCmd Text
        read'blogcontents pos _p _i n =
          getBlogContHtml pos n

        read'blogsource :: Int -> ObjCmd Text
        read'blogsource pos _p _i n =
          getBlogCont pos n

        read'previewref :: (Int, GeoAR) -> ObjCmd (Path, Int, GeoAR)
        read'previewref (pos, geo) p _i _n = return (p, pos, geo)

        read'metadata :: Int -> ObjCmd (Path, Int)
        read'metadata pos p _i _n = return (p, pos)

        read'rating :: Int -> ObjCmd (Path, Int)
        read'rating pos p _i _n = return (p, pos)

        read'ratings :: ObjCmd Path
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
--
-- helper for blog entry ops

getBlogContHtml :: Int -> ImgNode -> Cmd Text
getBlogContHtml =
  processColEntryAt
    (\ i nm -> getColBlogCont i nm)       -- ImgRef: entry is a blog text
    (\ i    -> do                         -- ColRef: lookup the col blog ref
        be <- getImgVals i theColBlog
        maybe (return mempty)             -- return nothing, when not there
              (uncurry getColBlogCont)    -- else generate the HTML
              be
    )

getBlogCont :: Int -> ImgNode -> Cmd Text
getBlogCont =
  processColEntryAt
    (\ i nm -> getColBlogSource i nm)
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

-- ----------------------------------------
