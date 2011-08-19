{-# LANGUAGE QuasiQuotes #-}

-- ------------------------------------------------------------

-- tcl lib for initializing global variables argv argc argv0 and env

-- ------------------------------------------------------------

module Language.Tcl.Commands.Env
    ( tclEnvLib
    )
where

import Control.Arrow                   ( first )
import Control.Monad.RWS

import Language.Tcl.Core
import Language.Tcl.QuasiQuote
import Language.Tcl.Value

import System.Environment

-- ------------------------------------------------------------

tclEnvLib :: TclLib e s
tclEnvLib = (tclEnvInit, tclEnvCommands)

tclEnvCommands :: [(String, TclCommand e s)]
tclEnvCommands
    = map (first (nsp ++))
      [ ("getArgs",        tclGetArgs)
      , ("getProgName",    tclGetProgName)
      , ("getEnvironment", tclGetEnvironment)
      ]

-- ------------------------------------------------------------

tclEnvInit :: TclEval e s ()
tclEnvInit
    = interpreteTcl initScript >> return ()

initScript :: String
initScript		-- edit tcl_ prefix if nsp is changed
    = [tcl|
       set tcl_traceLevel 1

       #@tclEnvInit: tracing of "#@..." is enabled for testing, change default in "Env.hs"
       #@tclEnvInit: init argc, argv and argv0

       set argv0 [tcl_getProgName]

       set argv  [tcl_getArgs]
       set argc  [llength $argv]

       #@tclEnvInit: initializing env array

       foreach tcl_kvp [tcl_getEnvironment] {
                     lassign $tcl_kvp tcl_k tcl_v
                     eval [list set env($tcl_k) $tcl_v]
                   }
       unset tcl_kvp tcl_k tcl_v

       #@tclEnvInit: init script finished
       |]

-- ------------------------------------------------------------

tclGetArgs :: TclCommand e s
tclGetArgs []
    = do args <- liftIO getArgs
         return $ mkL $ map mkS args

tclGetArgs _
    = tclWrongArgs $ (nsp ++) "getArgs"

-- ------------------------------------------------------------

tclGetProgName :: TclCommand e s
tclGetProgName []
    = do pn <- liftIO getProgName
         return $ mkS pn

tclGetProgName _
    = tclWrongArgs $ (nsp ++) "getProgName"

-- ------------------------------------------------------------

tclGetEnvironment :: TclCommand e s
tclGetEnvironment []
    = do env <- liftIO getEnvironment
         return $ mkL $ map (\ (n, v) -> mkL [mkS n, mkS v]) env

tclGetEnvironment _
    = tclWrongArgs $ (nsp ++) "getEnvironment"

-- ------------------------------------------------------------
