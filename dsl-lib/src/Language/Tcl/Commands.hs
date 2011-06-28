module Language.Tcl.Commands
where

import Language.Tcl.Core

import Language.Tcl.Commands.Expr       ( tclExpr
                                        , tclIncr
                                        )
import Language.Tcl.Commands.IfForWhile ( tclFor
                                        , tclForeach
                                        , tclIf
                                        , tclWhile
                                        )
import Language.Tcl.Commands.List       ( tclLappend
                                        , tclLindex
                                        , tclLinsert
                                        , tclList
                                        , tclLlength
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
      , ("for",         tclFor)
      , ("foreach",     tclForeach)
      , ("if",          tclIf)
      , ("incr",        tclIncr)
      , ("lappend",     tclLappend)
      , ("lindex",      tclLindex)
      , ("linsert",     tclLinsert)
      , ("list",        tclList)
      , ("llength",     tclLlength)
      , ("puts", 	tclPuts)
      , ("return",      tclReturn)
      , ("set",		tclSet)
      , ("while",       tclWhile)
      ]

-- ------------------------------------------------------------
