module Language.Tcl.Eval
    ( module Language.Tcl.Eval
    , module Language.Tcl.Core
    , module Language.Tcl.Commands
    , module Language.Tcl.CheckArgs
    )
where

import qualified
       Data.Map               		as M

import Language.Tcl.Core
import Language.Tcl.Commands
import Language.Tcl.CheckArgs

import System.IO

-- ------------------------------------------------------------

initTclEnv	:: e -> TclEnv e
initTclEnv e
    = TclEnv
      { _appEnv = e
      }

initTclState :: s -> TclState e s
initTclState s
    = TclState
      { _tglobalVars = M.empty
      , _tstack      = []
      , _tcmds       = M.fromList buildInTclCommands
      , _tprocs      = M.empty
      , _tchans      = M.fromList buildInTclChannels
      , _appState    = s
      }

buildInTclChannels :: [(String, Handle)]
buildInTclChannels
    = [ ("stdout", stdout)
      , ("stdin",   stdin)
      , ("stderr", stderr)
      ]

-- ------------------------------------------------------------
