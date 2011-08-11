module Language.Tcl.Commands
where

import Language.Tcl.Core

import Language.Tcl.Commands.Expr       ( tclExpr
                                        , tclIncr
                                        )
import Language.Tcl.Commands.IfForWhile ( tclFor
                                        , tclForeach
                                        , tclIf
                                        , tclSwitch
                                        , tclWhile
                                        )
import Language.Tcl.Commands.Info       ( tclInfo
                                        )
import Language.Tcl.Commands.List       ( tclLappend
                                        , tclLindex
                                        , tclLinsert
                                        , tclList
                                        , tclLlength
                                        , tclLrange
                                        , tclLreplace
                                        )
import Language.Tcl.Commands.ListSort   ( tclLsort
                                        )
import Language.Tcl.Commands.Puts       ( tclPuts
                                        )
import Language.Tcl.Commands.Return     ( tclBreak
                                        , tclContinue
                                        , tclError
                                        , tclProc
                                        , tclReturn
                                        )
import Language.Tcl.Commands.SetAppend  ( tclAppend
                                        , tclSet
                                        , tclUnset
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
      , ("info",        tclInfo)
      , ("lappend",     tclLappend)
      , ("lindex",      tclLindex)
      , ("linsert",     tclLinsert)
      , ("list",        tclList)
      , ("llength",     tclLlength)
      , ("lrange",      tclLrange)
      , ("lreplace",    tclLreplace)
      , ("lsort",       tclLsort)
      , ("proc", 	tclProc)
      , ("puts", 	tclPuts)
      , ("return",      tclReturn)
      , ("set",		tclSet)
      , ("switch",	tclSwitch)
      , ("unset",       tclUnset)
      , ("while",       tclWhile)
      ]

-- ------------------------------------------------------------
