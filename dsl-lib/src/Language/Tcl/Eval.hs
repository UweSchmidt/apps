module Language.Tcl.Eval
    ( module Language.Tcl.Eval
    , module Language.Tcl.Core
    , module Language.Tcl.Commands
    , module Language.Tcl.CheckArgs
    )
where

import Language.Tcl.CheckArgs
import Language.Tcl.Commands            ( tclCoreLib )
import Language.Tcl.Commands.Env        ( tclEnvLib )
import Language.Tcl.Core

import System.IO                        ( hPutStrLn
                                        , stderr
                                        )

-- ------------------------------------------------------------

initTclEnv	:: TclEnv e
initTclEnv
    = TclEnv
      { _tclLogger = hPutStrLn stderr
      , _appEnv = undefined	-- must be set when app is initialized
      }

setTclLogger	:: TclLogger -> TclEnv e -> TclEnv e
setTclLogger l e
    = e { _tclLogger = l }

initTclState :: TclState e s
initTclState
    = TclState
      { _tglobalVars = emptyTclVars
      , _tstack      = []
      , _tcmds       = emptyTclCommands
      , _tprocs      = emptyTclProcs
      , _tchans      = emptyTclChannels
      , _appState    = undefined	-- must be set when app is initialized
      }

-- ------------------------------------------------------------

initTcl :: TclEval e s ()
initTcl
    = sequence_ $
      map loadTclLib
      [ tclCoreLib
      , tclEnvLib
      ]

-- ------------------------------------------------------------
