{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Catalog.Cmd (Env, Cmd, runAction, initEnv, initState, envPort, envVerbose)
import           Data.Prim.Prelude ((^.))
import           Data.ImageStore (ImgStore)
import           Network.HTTP.Types.Status -- (internalServerError500, Status, notFound404, ok200, status403)
import qualified Network.Wai.Handler.Warp as W
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           System.Exit (die)
import           Web.Scotty --                           (middleware, scotty)
import Web.Scotty.Internal.Types
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Except

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

  let runRead = runReadCmd env mvRead
  let runMody = runModyCmd env mvRead mvMody

  -- start scotty
  opts <- getOptions env
  scottyOpts opts $ do
--    middleware $ staticPolicy (noDots >-> addBase "static/images") -- for favicon.ico
    middleware logStdoutDev
    -- defined routes
    get "/" $ text "the home page"
    get "/help" $ text "help not yet available"
    get "/emil" $ do
      text "emil not at home"
      status status403

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
