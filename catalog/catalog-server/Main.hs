{-# LANGUAGE OverloadedStrings #-}

module Main where

import ServerOptions(mainWithArgs)
import           Catalog.Cmd (Env, Cmd, runAction, initState, envPort, envVerbose, envMountPath)
import           Catalog.System.Convert (genImage)
import           Catalog.Html.Photo2 (genHtmlPage)
import           Control.Concurrent.MVar
import           Control.Exception (SomeException, try, catch, toException)
import           Control.Monad.Except
import           Control.Monad.IO.Class
import qualified Control.Monad.RWSErrorIO as RWS
import           Data.ImageStore (ImgStore)
import           Data.Monoid ((<>))
import           Data.Prim.Prelude -- (Text, (^.), isoString, isoText, from, to, intercalate)
import           Data.Prim.Constants
import qualified Data.Text as T
import           Network.HTTP.Types.Status -- (internalServerError500, Status, notFound404, ok200, status403)
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Exit (die)
import           Web.Scotty --                           (middleware, scotty)
import           Web.Scotty.Internal.Types

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
        (\ e -> return (Left . RWS.Msg . show $ (e :: SomeException)))
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> ActionM a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either (throwError . stringError . show) return res
  where
    runc = do
      store <- takeMVar mvm
      (res, new'store, _log) <- runAction cmd env store

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
matchHTML = matchPath $ ps'html ^. isoText
  where
    ps'html =
      "/[a-zA-Z]+-[0-9]+x[0-9]+"
      ++
      ps'collections
      ++
      "(/.*)?[.]html"

matchJPG :: RoutePattern
matchJPG = matchPath "/.*[.]jpg"

-- ----------------------------------------

fileWithMime :: FilePath -> Text -> FilePath -> ActionM ()
fileWithMime px mt fp = do
  setHeader "Content-Type" (mt ^. lazy)
  file (px ++ fp)

-- ----------------------------------------

main :: IO ()
main = mainWithArgs $ \ env -> do
  est  <- initState env
  either die (main' env) est

main' :: Env -> ImgStore -> IO ()
main' env state = do
  -- create MVars for the image archive state
  mvRead <- newMVar state
  mvMody <- newMVar state

  let runRead  = runReadCmd env mvRead
  let runMody  = runModyCmd env mvRead mvMody

  let mimeFile = fileWithMime (env ^. envMountPath)

  -- start scotty
  opts <- getOptions env
  scottyOpts opts $ do
--    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    -- defined routes
    get "/" $ text "the home page"
    get "/help" $ text "help not yet available"

    get matchJS $ do
      param "path" >>= mimeFile "text/javascript"

    get matchCSS $ do
      param "path" >>= mimeFile "text/css"

    get matchHTML $ do
      p <- param "path"
      res <- runRead $ genHtmlPage p
      html (res ^. lazy)

    get matchJPG $ do
      p <- param "path"
      f <- runRead $ genImage p
      fileWithMime "" "image/jpeg" f

    get (matchPath "/.*[.]ico") $ do
      mimeFile "image/x-icon" "/assets/icons/favicon.ico"

    get (matchPath ".*") $ do
      p <- param "path"
      text $ "file " <> p <> "not in archive"
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
