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
import Language.Tcl.Value

-- ------------------------------------------------------------

initTclEnv	:: e -> TclEnv e
initTclEnv e
    = TclEnv
      { _appEnv = e
      }

initTclState :: s -> TclState e s
initTclState s
    = TclState
      { _tglobalVars = emptyTclVars
      , _tstack      = []
      , _tcmds       = emptyTclCommands
      , _tprocs      = emptyTclProcs
      , _tchans      = emptyTclChannels
      , _appState    = s
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
