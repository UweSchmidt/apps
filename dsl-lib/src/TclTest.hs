{-# OPTIONS -XTypeSynonymInstances  -XFlexibleContexts -XFlexibleInstances -XMultiParamTypeClasses #-}

module TclTest
where

import Control.Arrow -- (first, second, (&&&), (***))
import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Data.List ( isPrefixOf )

import Language.Common.Eval
import Language.Common.EvalOptions

import Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser as P
import Language.Tcl.Show
import Language.Tcl.Eval
import Language.Tcl.Value

import Text.Parsec

-- ------------------------------------------------------------

type TclAppState e s
    = AppState (TclEnv e) (TclState e s)

initTclAppState :: (TclEnv e -> TclEnv e) ->
                   (TclState e s -> TclState e s) ->
                   IO (Either String (TclAppState e s))
initTclAppState configEnv configState
    = either (Left . show) Right <$>
      initApp (configEnv initTclEnv) (configState initTclState) initTcl
{-
    = do (res, state1, _wrt) <- runEval initTcl env0 state0
         case res of
           Left err -> return . Left $ show err
           Right _  -> Right <$> (initAppState env0 state1)
    where
      env0   = configEnv   initTclEnv
      state0 = configState initTclState
-}
-- ------------------------------------------------------------

type TestEval
    = TclEval () ()

-- ------------------------------------------------------------

execTcl	:: String -> IO ()
execTcl s
    = do (r, _st) <- runEval
                     (initTcl >> interpreteTcl s)
                     (initTclEnv { _appEnv = ()})
                     (initTclState { _appState = ()})
         putStrLn (show r)
         -- putStrLn (show st)

testTcl :: Error err => Eval err (TclEnv ()) (TclState e ()) res -> IO (Either err res, TclState e ())
testTcl s
    = runEval s
      (initTclEnv { _appEnv = ()})
      (initTclState { _appState = ()})


-- ------------------------------------------------------------
