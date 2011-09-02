{- {-# OPTIONS -XGeneralizedNewtypeDeriving #-} -}

module Language.Common.Eval
where

import Control.Applicative      ( (<$>) )

import Control.Concurrent.MVar

import Control.Exception       	( SomeException, try )

import Control.Monad.Error
import Control.Monad.RWS.Lazy

-- ------------------------------------------------------------
--
-- | record for storing the env and state of a program run
-- to enable continuation of execution

data EnvAndState env st
    = EnvAndState
      { _theEnv   :: env
      , _theState :: st
      }

newtype AppState env st
    = AppState (MVar (EnvAndState env st))

newAppState :: env -> st -> IO (AppState env st)
newAppState e0 s0
    = AppState <$> newMVar (EnvAndState e0 s0)

-- ------------------------------------------------------------
--
-- intitialize an interpreter by executing an init action
-- applied to a given initial env and state
-- and store the resulting state and the env in a MVar.
-- The interpreter state can be hidden from the rest of a system,
-- but can be stored within an application.

initApp :: Error err =>
           env -> st -> Eval err env wrt st res -> IO (Either err (AppState env st))
initApp e0 s0 action0
    = do (res, s1, _wrt) <- runEval action0 e0 s0
         case res of
           Left err -> return (Left err)
           Right _  -> do v <- newAppState e0 s1
                          return (Right v)

-- | continue an interpreter by taking an application state an run
-- a command with this env and state, the resulting state is again stored
-- in the app state for further use

contApp :: Error err =>
           AppState env st -> Eval err env wrt st res -> IO (Either err res)
contApp (AppState v) action
    = do es <- takeMVar v
         (res, s1, _wrt) <- runEval action (_theEnv es) (_theState es)
         putMVar v (es {_theState = s1})
         return res

-- ------------------------------------------------------------

-- | The simple case:
-- take a prog and an interpreter, and start running the prog with an initial env an state
-- which may be modified by an initializing action

runAppScript :: (Error err, Monoid wrt) =>
                env -> st -> Eval err env wrt st a ->
                (prg -> Eval err env wrt st res) ->
                prg ->
                IO (Either err res)
runAppScript e0 s0 init0 interpreter script
    = do (res, _s1, _wrt) <- runEval action e0 s0
         return res
    where
      action = init0 >> interpreter script

-- ------------------------------------------------------------
--
-- | Evaluation of an expression/command runs in an
-- error-reader-state-writer-IO monad

type Eval err env wrt st
    = ErrorT err (RWST env wrt st IO)

-- ------------------------------------------------------------
--
-- run the monad

runEval :: (Error err) =>
           Eval err env wrt st res ->
           env -> st ->
           IO (Either err res, st, wrt)

runEval expr env st 
    = runRWST (runErrorT expr) env st

-- ------------------------------------------------------------

liftIOE	:: (Error err, Monoid wrt) =>
           IO res -> Eval err env wrt st res
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

finallyError :: (Monoid wrt, Error err) =>
                Eval err env wrt st res -> Eval err env wrt st () -> Eval err env wrt st res
finallyError act sequel
    = do a <- act `catchError` (\ e -> sequel >> throwError e)
         _ <- sequel
         return a

-- ------------------------------------------------------------
