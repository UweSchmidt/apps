{-# LANGUAGE OverloadedStrings #-}

module Main where

import Catalog.Cmd (Env, Cmd
                   , runAction
                   , initState
                   , envPort
                   , envVerbose
                   , envMountPath
                   , envLogOp
                   )
import Catalog.FilePath (jpgPath)
import Catalog.Html.Photo2 (genHtmlPage)
import Catalog.Html.Blaze2 (genBlazeHtmlPage)
import Catalog.Options (mainWithArgs)
import Catalog.System.Convert (genImage, genImageFromTxt)

import Control.Concurrent.QSem
import Control.Exception.Base (bracket_)
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch) -- , try, toException)
import Control.Monad.Except
import qualified Control.Monad.ReaderStateErrIO as RSE

import Data.ImageStore (ImgStore)
import Data.Monoid ((<>))
import Data.Prim.Constants
import Data.Prim.Prelude
import qualified Data.Text as T

import Network.HTTP.Types.Status -- (internalServerError500, Status, notFound404, ok200, status403)
import Network.Wai
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import System.IO (hPutStrLn, hFlush, stderr)
import System.Exit (die)
import System.FilePath (dropExtension)

import Web.Scotty --                           (middleware, scotty)
import Web.Scotty.Internal.Types

import Json (jsonRPC)

-- import           Network.Wai.Middleware.Static        (addBase, noDots,
--                                                       staticPolicy, (>->))

-- ----------------------------------------

-- there are 2 mvars for the image store
--
-- one for reading operations, those can run in parallel
-- so the mvar is not emptied when starting an action
--
-- the 2. is for modifying the store, those operations run sequentially
-- and at the end they update both mvars wit the new state

runReadCmd :: Env -> MVar ImgStore -> Cmd a -> ActionM a
runReadCmd env mvs cmd = do
  res <- liftIO runc
  either (\ e -> raise (show e ^. isoText . lazy)) return res
  where
    runc = do
      store <- readMVar mvs
      res <-
        ((^. _1) <$> runAction cmd env store)
        `catch`
        -- TODO this still does not catch: error "some error"
        (\ e -> return (Left . RSE.Msg . show $ (e :: SomeException)))
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> ActionM a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either (throwError . stringError . show) return res
  where
    runc = do
      store <- takeMVar mvm
      (res, new'store) <- runAction cmd env store

      -- TODO try to catch: error "some error" like in runReadCmd

      _old <- swapMVar mvr new'store
      putMVar mvm new'store
      return res

-- ----------------------------------------

reqPath :: Request -> Text
reqPath req = "/" <> (T.intercalate "/" $ pathInfo req)

reqMatch :: RegexText -> Request -> Maybe [Param]
reqMatch e req
  | matchRE e path = Just [("path", path ^. from strict)]
  | otherwise = Nothing
  where
    path = reqPath req

matchPath :: Text -> RoutePattern
matchPath e = function $ reqMatch (parseRegexExt e)

matchJS :: RoutePattern
matchJS = matchPath $ (ps'javascript ++ "/.*[.]js") ^. isoText

matchCSS :: RoutePattern
matchCSS = matchPath $ (ps'css ++ "/.*[.]css") ^. isoText

matchHTML :: RoutePattern
matchHTML = matchPath $ pshtml ^. isoText
  where
    pshtml =
      "/html-[0-9]+x[0-9]+"
      ++
      ps'collections
      ++
      "(/.*)?[.]html"

matchBlazeHTML :: RoutePattern
matchBlazeHTML = matchPath $ pshtml ^. isoText
  where
    pshtml =
      "/blaze-[0-9]+x[0-9]+"
      ++
      ps'collections
      ++
      "(/.*)?[.]html"

matchJPG :: RoutePattern
matchJPG = matchPath $ jpgPath ^. isoText

matchTXT :: RoutePattern
matchTXT = matchPath "/.*[.](txt|md)[.]jpg"

matchZIP :: RoutePattern
matchZIP = matchPath "/.*[.]zip"

matchBootstrap :: Text -> RoutePattern
matchBootstrap ext = matchPath ("/bootstrap/.*[.]" `T.append` ext)

matchJsonGet :: RoutePattern
matchJsonGet = matchPath $ "/get-[a-zA-Z0-9]+/" <> t'archive <> "/.*[.]json"

matchJsonModify :: RoutePattern
matchJsonModify = matchPath $ "/modify-[a-zA-Z0-9]+/" <> t'archive <> "/.*[.]json"

matchJsonOther :: RoutePattern
matchJsonOther = matchPath ".*[.]json"


-- match for old Photo2 album pages
matchPhoto2 :: Text -> RoutePattern
matchPhoto2 ext = matchPath ("/Alben/.*[.]" `T.append` ext)

-- ----------------------------------------

fileWithMime :: FilePath -> Text -> FilePath -> ActionM ()
fileWithMime px mt fp = do
  setHeader "Content-Type" (mt ^. lazy)
  file (px ++ fp)

-- ----------------------------------------

main :: IO ()
main = mainWithArgs "server" $ \ env -> do
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
main' env state = do
  -- create MVars for the image archive state
  mvRead <- newMVar state
  mvMody <- newMVar state

  let runRead  = runReadCmd env mvRead
  let runMody  = runModyCmd env mvRead mvMody

  let mimeFile  = fileWithMime (env ^. envMountPath)
  let text'html = "text/html;charset=utf-8"

  -- start scotty
  opts <- getOptions env
  scottyOpts opts $ do
--    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev

    -- ----------------------------------------
    -- the routes for photo edit and AJAX calls
    -- default route

    get "/" $
      mimeFile text'html $ ps'html ++ "/index.html"

    get "/index.html" $
      mimeFile text'html $ ps'html ++ "/index.html"

    -- the edit page
    get "/edit.html" $
      mimeFile text'html $ ps'html ++ "/edit.html"

    post "/get.json" $ do
      d   <- jsonData
      res <- runRead $ jsonRPC d
      json res

    post "/modify.json" $ do
      d   <- jsonData
      res <- runMody $ jsonRPC d
      json res

    -- ----------------------------------------
    -- bootstrap routes

    sequence_ $
      map (\ (ext, mty) ->
            get (matchBootstrap ext) $
            param "path" >>= mimeFile mty
          )
      [ ("js",           "text/javascript")
      , ("html",         text'html)
      , ("css([.]map)?", "text/css")
      , ("svg",          "image/svg+xml")
      , ("ttf",          "application/x-font-ttf")
      , ("woff",         "application/x-font-woff")
      , ("woff2",        "application/font-woff2")
      , ("woff2",        "application/vnd.ms-fontobject")
      ]

    -- ----------------------------------------
    -- old Phot2 routes

    sequence_ $
      map (\ (ext, mty) ->
            get (matchPhoto2 ext) $
            param "path" >>= mimeFile mty
          )
      [ ("js",   "text/javascript")
      , ("html", text'html)
      , ("css",  "text/css")
      , ("jpg",  "image/jpg")
      ]

    -- ----------------------------------------
    -- the routes for the slide show

    -- css and javascript for slide show
    get matchCSS $ do
      param "path" >>= mimeFile "text/css"

    get matchJS $ do
      param "path" >>= mimeFile "text/javascript"

    get matchZIP $ do
      param "path" >>= mimeFile "application/zip"

    -- HTML page for collections and images
    get matchHTML $ do
      p <- param "path"
      res <- runRead $ genHtmlPage p
      html (res ^. lazy)

    -- HTML page for collections and images generated with Blaze
    get matchBlazeHTML $ do
      p <- param "path"
      res <- runRead $ genBlazeHtmlPage p
      html res

    -- ----------------------------------------
    -- routes for images

    -- icon preview for text files
    -- must be checked before matchJPG
    get matchTXT $ do
      p <- param "path"
      f <- runRead $ genImageFromTxt (dropExtension p)
      fileWithMime "" "image/jpeg" f

    -- jpg image
    get matchJPG $ do
      p <- param "path"
      f <- runRead $ genImage p
      fileWithMime "" "image/jpeg" f

    -- favicon
    get (matchPath "/.*[.]ico") $ do
      mimeFile "image/x-icon" "/assets/icons/favicon.ico"

    -- ----------------------------------------

    -- the test page
    get "/test.html" $
      mimeFile text'html "/test.html"

    -- not found route
    get (matchPath ".*") $ do
      p <- param "path"
      text $ "file " <> p <> " not in archive"
      status status404

    -- test, test, test
    get (matchPath "/[a-zA-Z]+-[0-9]+x[0-9]+/.*[.]jpg") $ do
      p <- param "path"
      html $ "<html><head></head><body><h1>image href found: " <> p <> "</body></html>"

    -- test, test, test
    get (function $
         \ req -> Just [("xxx", pathInfo req ^. to show . from isoString)]
        ) $ do
      x <- param "xxx"
      text x

-- ----------------------------------------

splitJsonFct :: Text -> (Text, Text)
splitJsonFct p
  | [("fct", fct), ("path", path)] <- matchSubexRE splitJsonRE p =
      (fct, path)
  | otherwise =
      ("", p)

splitJsonRE :: RegexText
splitJsonRE = parseRegexExt "/({fct}[^/]+)({path}/.*)[.]json"

-- ----------------------------------------

getOptions :: Env -> IO Options
getOptions env = do
  let port = env ^. envPort
  let verb = fromEnum $ env ^. envVerbose
  return $
    Options
    { verbose  = verb
    , settings = W.setPort port $ W.defaultSettings
    }

-- ----------------------------------------
