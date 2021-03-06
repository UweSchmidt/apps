{-# OPTIONS #-}

module Language.Tcl.Shell
where

import Control.Monad.Error

import Data.List                  ( isInfixOf )
import Data.Maybe                 ( fromMaybe )

import Language.Common.Eval
import Language.Common.REPL

import Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser as P
import Language.Tcl.Eval
import Language.Tcl.Value
import Language.Tcl.Commands.SetAppend     ( tclSource )

import System.Exit                ( exitFailure )

-- ------------------------------------------------------------

type TclAppState e s
    = AppState (TclEnv e) (TclState e s)

type TclREPLState env state
    = REPLState TclProg (TclAppState env state) Value TclError

--
-- initialize a Tcl REPL
-- The initial application env end state must be configured,
-- this can be parameterized, and the Tcl interpreter must be initialized,
-- and the REPL state must be configured

initTclREPLState ::  (TclEnv e -> TclEnv e)         ->
                     (TclState e s -> TclState e s) ->
                     TclEval e s r                  ->
                     TclREPLState e s               ->
                     IO (TclREPLState e s)
initTclREPLState configEnv configState initAppScript replState
    = do ires <- initApp
                 (configEnv initTclEnv)
                 (configState initTclState)
                 (initTcl >> initAppScript)
         case ires of
           Left err -> ( message .
                         ("initialization failed: " ++) .
                         show $ err
                       )
                       >> exitFailure
           Right s0 -> return $
                       replState { _appstate = s0 }

defaultTclREPLState :: TclREPLState env state
defaultTclREPLState
    = REPLState
      { _hello    = return "Haskell TclShell version -1.0"
      , _bye      = return "bye"
      , _prompt   = return "tclsh:\\w(\\#)> "
      , _prompt2  = return " > "
      , _readline = readLine
      , _parse    = return . tclParse
      , _eval     = eval
      , _printVal = printVal
      , _printErr = printErr
      , _finished = finished
      , _cmdcnt   = 0
      , _appstate = undefined	-- this hole must be filled later in initTclREPLState
      }
    where
      eval p s
          = liftIO $ contApp s (evalTclProg p)

      printVal
          = printRes . selS

      printErr (TclError lev msg)
          | lev == (-1)			-- exit program when level is -1, rc is found in msg
              = repLoopExit rc
          | otherwise
              = mess
          where
            rc = fromInteger . fromMaybe 2 . selI $ msg
            mess = message (selS msg)

      tclParse s
          = case P.parseTclProg s of
              Left err
                  ->  evalErr err
              Right p
                  -> if null (_tclProg p)
                     then Noop
                     else Prog p
          where
            evalErr e0
                | "unexpected end of input" `isInfixOf` msg
                    = Incomplete s
                | otherwise
                    = SyntaxErr msg
                where
                  msg = show e0

-- ------------------------------------------------------------
--
-- the simplest way to run a Tcl shell

type TclshREPLState = TclREPLState () ()

-- tclsh does not have any application env or state
-- for other apps initialize appEnv and appState with values of app specific types

runTclShell :: Maybe String -> env -> state -> IO ()
runTclShell initScript e0 s0
    = do s1 <- initTclREPLState
               (\ e -> e {_appEnv   = e0 })
               (\ s -> s {_appState = s0 })
               (maybe (return mempty) loadScript initScript)
               defaultTclREPLState
         runREPL repLoop0 s1
    where
      loadScript fn
          = tclSource [mkS fn]

runTclScript :: Maybe FilePath -> env -> state -> IO ()
runTclScript script e0 s0
    = do (res, _s) <- runEval runScript
                      (initTclEnv   {_appEnv   = e0 })
                      (initTclState {_appState = s0 })
         case res of
           Left (TclError lev msg)
               -> if lev == (-1)
                  then finished $ (fromInteger . fromMaybe 2 . selI $ msg)
                  else message (selS msg) >> finished 1
           Right _
               -> finished 0
    where
      runScript
          = do initTcl
               sc <- liftIOE $ maybe getContents readFile script
               interpreteTcl sc

-- ------------------------------------------------------------
