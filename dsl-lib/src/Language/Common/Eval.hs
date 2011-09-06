{- {-# OPTIONS -XGeneralizedNewtypeDeriving #-} -}

module Language.Common.Eval
where

import Control.Applicative      ( (<$>) )

import Control.Concurrent.MVar

import Control.Exception       	( SomeException, try )

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

-- ------------------------------------------------------------
--
-- | Evaluation of an expression/command runs in an
-- error-reader-state-IO monad

type Eval err env state
    = ErrorT err (ReaderT env (StateT state IO))

runEval :: (Error err) =>
           Eval err env state res ->
           env -> state ->
           IO (Either err res, state)
runEval expr e0 s0 
    = runStateT (runReaderT (runErrorT expr) e0) s0

-- ------------------------------------------------------------
--
-- | record for storing the env and state of a program run
-- to enable later continuation of execution

data EnvAndState env state
    = EnvAndState
      { _theEnv   :: env
      , _theState :: state
      }

newtype AppState env state
    = AppState (MVar (EnvAndState env state))

-- ------------------------------------------------------------
--
-- intitialize an interpreter by executing an init action
-- applied to a given initial env and state
-- and store the resulting state and the env in a MVar.
-- The interpreter state can be hidden from the rest of a system,
-- but can be stored within an application.

initApp :: Error err =>
           env -> state -> Eval err env state res -> IO (Either err (AppState env state))
initApp e0 s0 action0
    = do (res, s1) <- runEval action0 e0 s0
         case res of
           Left err -> return (Left err)
           Right _  -> do v <- newAppState e0 s1
                          return (Right v)

newAppState :: env -> state -> IO (AppState env state)
newAppState e0 s0
    = AppState <$> newMVar (EnvAndState e0 s0)

-- | continue an interpreter by taking an application state an run
-- a command with this env and state, the resulting state is again stored
-- in the app state for further use

contApp :: Error err =>
           AppState env state -> Eval err env state res -> IO (Either err res)
contApp (AppState v) action
    = do es <- takeMVar v
         (res, s1) <- runEval action (_theEnv es) (_theState es)
         putMVar v (es {_theState = s1})
         return res

-- ------------------------------------------------------------

-- | The simple case:
-- take a prog and an interpreter, and start running the prog with an initial env an state
-- which may be modified by an initializing action

runAppScript :: (Error err) =>
                env -> state -> Eval err env state a ->
                (prg -> Eval err env state res) ->
                prg ->
                IO (Either err res)
runAppScript e0 s0 init0 interpreter script
    = do (res, _s1) <- runEval action e0 s0
         return res
    where
      action = init0 >> interpreter script

-- ------------------------------------------------------------

liftIOE	:: (Error err) =>
           IO res -> Eval err env state res
liftIOE a
    = do r <- liftIO $ try' a
         case r of
           Left exc
               -> throwError $ strMsg $ show exc
           Right res
               -> return res
    where
      try' :: IO a -> IO (Either SomeException a)
      try' = try

finallyError :: (Error err) =>
                Eval err env state res -> Eval err env state () -> Eval err env state res
finallyError act sequel
    = do a <- act `catchError` (\ e -> sequel >> throwError e)
         _ <- sequel
         return a

-- ------------------------------------------------------------
