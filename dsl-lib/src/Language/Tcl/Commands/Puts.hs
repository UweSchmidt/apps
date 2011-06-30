module Language.Tcl.Commands.Puts
    ( tclPuts
    )
where

import Control.Monad.RWS

import Language.Common.Eval
import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value

import System.IO

-- ------------------------------------------------------------

tclPuts :: TclCommand e s
tclPuts l
    = do (nnl, l1) <- tclFromEither . evalOptions putOptions False $ l
         tclPuts' nnl (map selS l1)
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

putOptions :: OptParser [Value] Bool
putOptions
    = options [isOpt (== (mkS "-nonewline")) (const True)]

-- ------------------------------------------------------------
