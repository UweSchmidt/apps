{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception      (SomeException, catch) -- , try, toException)
import Control.Exception.Base (bracket_)
import Control.Monad.Except
import Control.Monad.ReaderStateErrIO (Msg(..))

import Network.Wai.Handler.Warp
import Network.Wai.Logger     (withStdoutLogger)

import Servant
import System.Directory       (doesFileExist)
import System.Exit            (die)
import System.IO              (hPutStrLn, stderr, hFlush)

import qualified Text.Blaze.Html      as Blaze
import qualified Data.ByteString.Lazy as LBS

-- catalog modules
import Data.Prim
import Data.ImgTree
import Data.ImageStore (ImgStore)

import Catalog.Cmd
import Catalog.Html.Blaze2
import Catalog.JsonCommands
import Catalog.Options        (mainWithArgs)
import Catalog.System.Convert (genImageGeo, genImageFromTxtGeo)

-- servant interface
import API

-- ----------------------------------------
--
-- conversions of [Text] to Path

ttp :: (Path -> a) -> ([Text] -> a)
ttp f xs = f (listToPath xs)

ttp2 :: (a -> Path -> b) -> (a -> [Text] -> b)
ttp2 = flip . ttp . flip

ttpSnd :: ((a, Path) -> b) -> ((a, [Text]) -> b)
ttpSnd f (x, ys) = f (x, listToPath ys)

-- ----------------------------------------
--
-- adapter for catalog commands to handler

pin :: ((ObjId, ImgNode) -> a) ->
       (a -> Cmd r) ->
       (Path -> Cmd r)
pin sel cmd path = do
  i'n <- getIdNode' path
  cmd $ sel i'n

mkcmd0 :: (Cmd r -> Handler r) ->
          (Path -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd0 toHandler pcmd =
  toHandler . pcmd . listToPath

mkcmd1 :: (Cmd r -> Handler r) ->
          (ObjId -> ImgNode -> Cmd r) ->
          ([Text] -> Handler r)
mkcmd1 toHandler pcmd =
  toHandler . pin id (uncurry pcmd) . listToPath

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

mkcmd2 :: (Cmd r -> Handler r) ->
          (a -> ObjId -> ImgNode -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2 toHandler pcmd path args =
  toHandler . (pin id $ uncurry $ pcmd args) . listToPath $ path

mkcmd2n :: (Cmd r -> Handler r) ->
           (a -> ImgNode -> Cmd r) ->
           ([Text] -> a -> Handler r)
mkcmd2n toHandler pcmd path args =
  toHandler . (pin snd $ pcmd args) . listToPath $ path

-- ----------------------------------------
-- the server

catalogServer :: Env ->
                 (forall a . Cmd a -> Handler a) ->
                 (forall a . Cmd a -> Handler a) ->
                 Server CatalogAPI
catalogServer env runR runM =
  ( bootstrap
    :<|>
    ( assets'css
      :<|>
      assets'icons
      :<|>
      assets'javascript
    )
    :<|>
    root'html
  )
  :<|>
  ( blaze
    :<|>
    imgcopy
  )
  :<|>
  ( json'read
    :<|>
    json'modify
  )
  where
    mountPath = env ^. envMountPath
    static p  = serveDirectoryWebApp (mountPath ++ p)

    bootstrap         = static "/bootstrap"
    assets'css        = static ps'css
    assets'icons      = static ps'icons
    assets'javascript = static ps'javascript

    -- root html files are located under /assets/html

    root'html :: BaseName HTMLStatic -> Handler LazyByteString
    root'html bn = staticFile ps'html bn

    staticFile :: FilePath -> BaseName a -> Handler LazyByteString
    staticFile dirPath (BaseName n) = do
      ex <- liftIO $ doesFileExist fp
      case ex of
        False ->
          throwError err404
        True ->
          liftIO (LBS.readFile fp)
      where
        fp = mountPath ++ dirPath ++ "/" ++ n ^. isoString

    blaze :: BlazeHTML -> [Text] -> Handler Blaze.Html
    blaze (BlazeHTML (Geo' geo)) ts = do
      case parseImgGeoPath geo fp of
        Nothing ->
          throwError $
          err404 { errBody = ("album page not found: " ++ fp) ^. from isoString }
        Just (pconf, colref) ->
          runR $ genBlazeHtmlPage' pconf colref

      where
        fp = concatMap (('/' :) . (^. isoString)) ts

    imgcopy (GeoAR' geo) =
      imgcopy'archive geo
      :<|>
      imgcopy'others  geo
      where
        imgcopy'archive = ttp2 imgcopy'
        imgcopy'others  = ttp2 imgcopy'

    imgcopy' :: GeoAR -> Path -> Handler LazyByteString
    imgcopy' geo path = do
      -- TODO: write an url parser like in blaze handler
      let fp = path ^. isoString
      let isJpg = checkExtPath     ".jpg" path
      let isTxt = checkExtPath ".txt.jpg" path
                  ||
                  checkExtPath  ".md.jpg" path
      ex <- liftIO $
            doesFileExist $ mountPath ++ fp
      case ex && isJpg of
        False ->
          throwError $
          err404 { errBody = ("image not found: " ++ fp) ^. from isoString }
        True -> do
          imgPath <- runR
                     ( ( if isTxt
                         then genImageFromTxtGeo
                         else genImageGeo
                       ) geo fp
                     )
          liftIO (LBS.readFile imgPath)

    mkR0  = mkcmd0  runR
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
      mkR2n (uncurry read'previewref)
      :<|>
      mkR2n read'metadata
      :<|>
      mkR2n read'rating
      :<|>
      mkR1n read'ratings
      where

    mkM1  = mkcmd1  runM
    mkM1i = mkcmd1i runM
    mkM2  = mkcmd2  runM
    mkM2i = mkcmd2i runM
    mkM2n = mkcmd2n runM

    json'modify =
      mkM2n (uncurry modify'saveblogsource)
      :<|>
      mkM2n (uncurry modify'changeWriteProtected)
      :<|>
      mkM2i modify'sort
      :<|>
      mkM2  modify'removeFromCollection
      :<|>
      mkM2n (uncurry modify'copyToCollection)
      :<|>
      mkM2  (uncurry modify'moveToCollection)
      :<|>
      mkM2i (uncurry modify'colimg)
      :<|>
      mkM2i (uncurry modify'colblog)
      :<|>
      mkM2i modify'newcol
      :<|>
      mkM2i modify'renamecol
      :<|>
      mkM2n (uncurry modify'setMetaData)
      :<|>
      mkM2n (uncurry modify'setMetaData1)
      :<|>
      mkM2n (uncurry modify'setRating)
      :<|>
      mkM2n (uncurry modify'setRating1)
      :<|>
      mkM2n (\t _i -> modify'snapshot t)
      :<|>
      mkM1i modify'syncCol
      :<|>
      mkM1i modify'syncExif
      :<|>
      mkM1i modify'newSubCols
      :<|>
      mkM1  modify'zipcollection

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
    -- a log command that syncronizes
    -- output of messages to stderr
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
      catalogServer env runRead runMody

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
      res <- ( (^. _1) <$> runAction cmd env store )
             `catch`
             -- TODO this still does not catch: error "some error"
             (\ e -> return (Left . Msg . show $ (e :: SomeException)))
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> Handler a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either raise500 return res
  where
    runc = do
      store <- takeMVar mvm
      res <- ( do
                 (res', new'store) <- runAction cmd env store
                 _old <- swapMVar mvr new'store
                 putMVar mvm new'store
                 return res'
             )
             `catch`
             (\ e -> return (Left . Msg . show $ (e :: SomeException)))
      return res

raise500 :: Msg -> Handler a
raise500 (Msg msg) =
  throwError $ err500 { errBody = msg' }
  where
    msg' = show msg ^. from isoString

-- ----------------------------------------
