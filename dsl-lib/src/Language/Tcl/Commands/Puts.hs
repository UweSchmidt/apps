module Language.Tcl.Commands.Puts
    ( tclPuts
    )
where

import Control.Monad.RWS

import Language.Common.Eval

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs

import System.IO

-- ------------------------------------------------------------

tclPuts :: TclCommand e s
tclPuts l
    = do (nnl, l1) <- tclOption1 "-nonewline" False True l
         tclPuts' nnl (map v2s l1)
         return mempty
      where
        tclPuts' nnl [s]
            = tclPuts' nnl ["stdout", s]

        tclPuts' nnl [cn, s]
            = do h <- get >>= lookupChannel cn
                 liftIOE $ ( if nnl
                             then hPutStr
                             else hPutStrLn) h s

        tclPuts' _ _
            = tclWrongArgs "puts ?-nonewline? ?channelId? string"

-- ------------------------------------------------------------
