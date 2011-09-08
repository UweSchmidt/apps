module Language.Tcl.Commands.Puts
    ( tclGets
    , tclPuts
    )
where

import Control.Applicative              ( (<$>) )
import Control.Monad.RWS

import Language.Common.Eval
import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value

import System.IO

-- ------------------------------------------------------------

tclGets :: TclCommand e s
tclGets [cn]
    = nextLine cn

tclGets [cn, var]
    = do l <- nextLine cn
         setVar (selVN var) l
         return (mkI . toInteger . length . selS $ l)

tclGets _
    = tclWrongArgs "gets channelId ?varName?"

nextLine :: Value -> TclEval e s Value
nextLine cn
    = do h <- get >>= lookupChannel (selS cn)
         mkS <$> (liftIOE $ hGetLine h)

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
    = options [isOpt ((== "-nonewline") . selS) (const True)]

-- ------------------------------------------------------------
