module Language.Tcl.Commands
    ( tclCoreLib )
where

import Language.Tcl.Core

import Language.Tcl.Commands.Array      ( tclArray
					)
import Language.Tcl.Commands.Cd         ( tclCd
                                        , tclPwd
					)
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
import Language.Tcl.Commands.List       ( tclJoin
                                        , tclLappend
                                        , tclLindex
                                        , tclLinsert
                                        , tclList
                                        , tclLlength
                                        , tclLrange
                                        , tclLreplace
                                        , tclSplit
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
                                        , tclConcat
                                        , tclEval
                                        , tclLassign
                                        , tclSet
                                        , tclUnset
                                        )
import Language.Tcl.Value

import System.IO

-- ------------------------------------------------------------

tclCoreLib :: TclLib e s
tclCoreLib = ( addBuildInChannels, buildInTclCommands )

-- ------------------------------------------------------------

buildInTclChannels :: [(String, Handle)]
buildInTclChannels
    = [ ("stdout", stdout)
      , ("stdin",   stdin)
      , ("stderr", stderr)
      ]

addBuildInChannels :: TclEval e s ()
addBuildInChannels
    = sequence_ $ map (uncurry setChannel) buildInTclChannels

-- ------------------------------------------------------------

buildInTclCommands :: [(String, TclCommand e s)]
buildInTclCommands
    = [ ("#",           const (return mempty))	-- throw away comments
      , ("append",      tclAppend)
      , ("array",       tclArray)
      , ("break",       tclBreak)
      , ("cd",          tclCd)
      , ("concat",      tclConcat)
      , ("continue",    tclContinue)
      , ("error",       tclError)
      , ("eval",        tclEval)
      , ("expr",        tclExpr)
      , ("for",         tclFor)
      , ("foreach",     tclForeach)
      , ("if",          tclIf)
      , ("incr",        tclIncr)
      , ("info",        tclInfo)
      , ("join",        tclJoin)
      , ("lappend",     tclLappend)
      , ("lassign",     tclLassign)
      , ("lindex",      tclLindex)
      , ("linsert",     tclLinsert)
      , ("list",        tclList)
      , ("llength",     tclLlength)
      , ("lrange",      tclLrange)
      , ("lreplace",    tclLreplace)
      , ("lsort",       tclLsort)
      , ("proc", 	tclProc)
      , ("puts", 	tclPuts)
      , ("pwd",         tclPwd)
      , ("return",      tclReturn)
      , ("set",		tclSet)
      , ("split",	tclSplit)
      , ("switch",	tclSwitch)
      , ("unset",       tclUnset)
      , ("while",       tclWhile)
      ]

-- ------------------------------------------------------------
