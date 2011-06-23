module Language.Tcl.Commands
where

import Language.Tcl.Core

import Language.Tcl.Commands.Expr       ( tclExpr
                                        , tclIncr
                                        )
import Language.Tcl.Commands.IfForWhile ( tclForeach
                                        , tclIf
                                        )
import Language.Tcl.Commands.Puts       ( tclPuts
                                        )
import Language.Tcl.Commands.Return     ( tclBreak
                                        , tclContinue
                                        , tclError
                                        , tclReturn
                                        )
import Language.Tcl.Commands.SetAppend  ( tclAppend
                                        , tclSet
                                        )

-- ------------------------------------------------------------

buildInTclCommands :: [(String, TclCommand e s)]
buildInTclCommands
    = [ ("append",      tclAppend)
      , ("break",       tclBreak)
      , ("continue",    tclContinue)
      , ("error",       tclError)
      , ("expr",        tclExpr)
      , ("foreach",     tclForeach)
      , ("if",          tclIf)
      , ("incr",        tclIncr)
      , ("puts", 	tclPuts)
      , ("return",      tclReturn)
      , ("set",		tclSet)
      ]

-- ------------------------------------------------------------
