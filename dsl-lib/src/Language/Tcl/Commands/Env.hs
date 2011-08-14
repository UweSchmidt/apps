-- ------------------------------------------------------------

-- tcl lib for initializing global variables argv argc argv0 and env

-- ------------------------------------------------------------

module Language.Tcl.Commands.Env
    ( tclEnvLib
    )
where

import Control.Arrow                   ( first )
import Control.Monad.RWS

-- import Language.Common.Eval
-- import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value

-- import System.IO
import System.Environment

-- ------------------------------------------------------------

namespaceEnv :: String
namespaceEnv = "env::"

tclEnvLib :: TclLib e s
tclEnvLib = (tclEnvInit, tclEnvCommands)

tclEnvCommands :: [(String, TclCommand e s)]
tclEnvCommands
    = map (first (namespaceEnv ++))
      [ ("getArgs",        tclGetArgs)
      , ("getProgName",    tclGetProgName)
      , ("getEnvironment", tclGetEnvironment)
      ]

-- ------------------------------------------------------------

tclEnvInit :: TclEval e s ()
tclEnvInit
    = interpreteTcl initScript >> return ()

initScript :: String
initScript
    = unlines
      [ "set argv0 [env::getProgName]"
      , ""
      , "set argv  [env::getArgs]"
      , "set argc  [llength $argv]"
      , ""
      , "foreach env::kvp [env::getEnvironment] {"
      , "  lassign $env::kvp env::k env::v"
      , "  eval [list set env($env::k) $env::v]"
--    , "  eval [list set [join [list env ( $env::k )] {}] $env::v]"	-- TODO: workaround for computed array index
      , "}"
      , "unset env::kvp env::k env::v"
      ]

-- ------------------------------------------------------------

tclGetArgs :: TclCommand e s
tclGetArgs []
    = do args <- liftIO getArgs
         return $ mkL $ map mkS args

tclGetArgs _
    = tclWrongArgs $ namespaceEnv ++ "getArgs"

-- ------------------------------------------------------------

tclGetProgName :: TclCommand e s
tclGetProgName []
    = do pn <- liftIO getProgName
         return $ mkS pn

tclGetProgName _
    = tclWrongArgs $ namespaceEnv ++ "getProgName"

-- ------------------------------------------------------------

tclGetEnvironment :: TclCommand e s
tclGetEnvironment []
    = do env <- liftIO getEnvironment
         return $ mkL $ map (\ (n, v) -> mkL [mkS n, mkS v]) env

tclGetEnvironment _
    = tclWrongArgs $ namespaceEnv ++ "getEnvironment"

-- ------------------------------------------------------------
