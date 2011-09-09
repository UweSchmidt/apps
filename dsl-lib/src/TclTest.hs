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
import System.Environment

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
tclsh
    = do args <- getArgs
         case args of
           [] -> do i <- hIsTerminalDevice stdin
                    if i
                       then runTclShell  Nothing () ()
                       else runTclScript Nothing () ()
           [fn] -> runTclScript (Just fn) () ()
           ["-i", fn] -> runTclShell (Just fn) () ()
           _ -> message "wrong args, usage: tclsh [[-i] script]" >>
                finished 1
         return ()

-- ------------------------------------------------------------

main :: IO ()
main = tclsh

-- ------------------------------------------------------------
