module Language.Tcl.Commands.Return
    ( tclBreak
    , tclContinue
    , tclError
    , tclReturn
    )
where

import Control.Monad.Error
import Control.Monad.RWS

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs

-- ------------------------------------------------------------

tclBreak :: TclCommand e s
tclBreak []
    = throwError $ tclBreakExc

tclBreak _
    = tclWrongArgs "break"

-- ------------------------------------------------------------

tclContinue :: TclCommand e s
tclContinue []
    = throwError $ tclContinueExc

tclContinue _
    = tclWrongArgs "continue"

-- ------------------------------------------------------------

tclError :: TclCommand e s
tclError [msg]
    = tclThrowError $ v2s msg

tclError _
    = tclWrongArgs "error message"

-- ------------------------------------------------------------

tclReturn :: TclCommand e s
tclReturn l
    = do (rc, l1) <- tclOption2 "-code" 2 (checkReturnCode .v2s) l
         tclReturn' rc l1
    where
      tclReturn' rc []
          = tclReturn' rc [mempty]
      tclReturn' rc l1@[v]
          | rc == 1
              = tclError l1
          | rc == 2
              = throwError $ tclReturnExc (v2s v)
          | rc == 3
              = tclBreak []
          | rc == 4
              = tclContinue []
          | otherwise
              = throwError $ tclOtherExc rc (v2s v)
      tclReturn' _ _
          = tclWrongArgs "return ?-code code? ?result?"

checkReturnCode :: String -> TclEval e s Int
checkReturnCode s
    = checkArg ("bad completion code "
		++ show s
		++ ": must be ok, error, return, break, continue, or an integer"
	       ) rc2i s
    where
    rc2i v
	= lookup v (zip ["ok", "error", "return", "break", "continue"] [0..])
          `mplus`
          readValue v

-- ------------------------------------------------------------
