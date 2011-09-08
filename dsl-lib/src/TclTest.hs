{-# OPTIONS -XTypeSynonymInstances  -XFlexibleContexts -XFlexibleInstances -XMultiParamTypeClasses #-}

module TclTest
where

import Control.Applicative        ( (<$>) )

import Data.List                  ( isInfixOf )
import Data.Maybe                 ( fromMaybe )

import Language.Common.Eval
import Language.Common.REPL

import Language.Tcl.Shell
import Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser as P
import Language.Tcl.Eval
import Language.Tcl.Value

import System.IO

-- ------------------------------------------------------------

initTclAppState :: (TclEnv e -> TclEnv e) ->
                   (TclState e s -> TclState e s) ->
                   IO (Either String (TclAppState e s))
initTclAppState configEnv configState
    = either (Left . show) Right <$>
      initApp (configEnv initTclEnv) (configState initTclState) initTcl

-- ------------------------------------------------------------

execTcl	:: String -> IO ()
execTcl s
    = do (r, _st) <- runEval
                     (initTcl >> interpreteTcl s)
                     (initTclEnv { _appEnv = ()})
                     (initTclState { _appState = ()})
         putStrLn (show r)
         -- putStrLn (show st)

-- ------------------------------------------------------------

tclsh :: IO ()
tclsh = runTclsh

-- ------------------------------------------------------------
