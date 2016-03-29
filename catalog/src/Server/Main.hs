{-# LANGUAGE OverloadedStrings #-}

module Server.Main where

import           Catalog.Cmd (Env, Cmd, runAction, initEnv, initState, envPort, envVerbose, envMountPath)
import           Catalog.Html.Photo2 (genHtmlPage)
import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.ImageStore (ImgStore)
import           Data.Monoid ((<>))
import           Data.Prim.Prelude -- (Text, (^.), isoString, isoText, from, to, intercalate)
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
  either (throwError . stringError . show) return res
  where
    runc = do
      store <- readMVar mvs
      (res, _store, _log) <- runAction cmd env store
      return res

runModyCmd :: Env -> MVar ImgStore -> MVar ImgStore -> Cmd a -> ActionM a
runModyCmd env mvr mvm cmd = do
  res <- liftIO runc
  either (throwError . stringError . show) return res
  where
    runc = do
      store <- takeMVar mvm
      (res, new'store, _log) <- runAction cmd env store
      _old <- swapMVar mvr new'store
      putMVar mvm new'store
      return res

-- ----------------------------------------

reqPath :: Request -> Text
reqPath req = "/" <> (T.intercalate "/" $ pathInfo req)

reqMatch :: RegexText -> Request -> Maybe [Param]
reqMatch re req
  | matchRE re path = Just [("path", path ^. from strict)]
  | otherwise = Nothing
  where
    path = reqPath req

matchPath :: Text -> RoutePattern
matchPath re = function $ reqMatch (parseRegexExt re)


-- ----------------------------------------

fileWithMime :: FilePath -> Text -> FilePath -> ActionM ()
fileWithMime ap mt fp = do
  setHeader "Content-Type" (mt ^. lazy)
  file (ap ++ fp)

-- ----------------------------------------

main :: IO ()
main = do
  env  <- initEnv
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

    get (matchPath "/assets/javascript/.*[.]js") $ do
      param "path" >>= mimeFile "text/javascript"

    get (matchPath "/assets/css/.*[.]css") $ do
      param "path" >>= mimeFile "text/css"

    get (matchPath "/assets/icons/favicon.ico") $ do
      param "path" >>= mimeFile "image/x-icob"

    get (matchPath "/[a-zA-Z]+-[0-9]+x[0-9]+/archive/collections(/.*)?[.]html") $ do
      p <- param "path"
      res <- runRead $ genHtmlPage p
      html (res ^. lazy)

    get (matchPath ".*") $ do
      p <- param "path"
      text $ "file " <> p <> "not in archive"
      status status404

    get (matchPath "/[a-zA-Z]+-[0-9]+x[0-9]+/.*[.]jpg") $ do
      p <- param "path"
      html $ "<html><head></head><body><h1>image href found: " <> p <> "</body></html>"

    get (function $ \ req -> Just [("xxx", pathInfo req ^. to show . from isoString)]) $ do
      x <- param "xxx"
      text x

-- ----------------------------------------


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
