module Language.Tcl.Commands.Return
    ( tclBreak
    , tclContinue
    , tclError
    , tclProc
    , tclReturn
    )
where

import Control.Applicative  ( (<$>) )
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
    = tclThrowError $ selS msg

tclError _
    = tclWrongArgs "error message"

-- ------------------------------------------------------------

tclReturn :: TclCommand e s
tclReturn l
    = do (rc, l1) <- tclOption2 "-code" 2 (checkReturnCode .selS) l
         tclReturn' rc l1
    where
      tclReturn' rc []
          = tclReturn' rc [mempty]
      tclReturn' rc l1@[v]
          | rc == 1
              = tclError l1
          | rc == 2
              = throwError $ tclReturnExc v
          | rc == 3
              = tclBreak []
          | rc == 4
              = tclContinue []
          | otherwise
              = throwError $ tclOtherExc rc v
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
          string2int v

-- ------------------------------------------------------------

tclProc :: TclCommand e s
tclProc (name' : params' : body : [])
    = do fparams <- checkListValue params' >>= mapM checkFParam
         let cpassing = paramPassing (usage name fparams) fparams
         cbody  <- evalTclProg <$> parseTclProg (selS body)
         let prc = TclProc
                   { _fparams  = mkL . map mkL $ fparams
                   , _fbody    = body
                   , _cbody    = cbody
                   , _cpassing = cpassing
                   }
         setProc name prc
         return mempty
    where
      name = selS name'
      checkFParam fp
          = do fpd <- checkListValue fp
               if length fpd > 2
                  then tclThrowError
                           $ "too many fields in argument specifier "
                             ++ show (selS fp)
                  else
                      if null . selS . head $ fpd
                         then tclThrowError
                                  $ "procedure "
                                    ++ show name
                                    ++ " has argument with no name"
                         else return fpd

tclProc _
    = tclWrongArgs "wrong # args: should be \"proc name args body\""

usage :: String -> [Values] -> TclEval e s Value
usage name fparams
    = tclThrowError msg
    where
      msg = "wrong # args: should be " ++ show (name ++ concatMap ((' ' :) . usage1) fparams)

      usage1 (n : [])     = selS n
      usage1 (n : _ : []) = "?" ++ selS n ++ "?"
      usage1 _            = error "usage called with illegal args"

paramPassing :: TclEval e s Value -> [Values] -> Values -> TclEval e s Value
paramPassing _use [] []
    = return mempty

paramPassing  use [] _				-- more actual than formal params: usage
    = use

paramPassing _use [(fp : _)] apl		-- last formal param has name "args", assign whole list to "args"
    | selS fp == "args"
        = setLocalVar "args" (mkL apl)

paramPassing  use ((fp : def : _) : fps) [] 	-- formal param with default, actual param list empty
    = setLocalVar (selS fp) def         	-- assign default value and process remaining formal params
      >>
      paramPassing use fps [] 

paramPassing  use ((_fp : []) : _) []		-- formal param without default, but no more actual params
    = use					-- usage

paramPassing  use ((fp : _) : fps) (ap1 : aps) 	-- formal param and actual param there
    = setLocalVar (selS fp) ap1	         	-- assign value and process remaining formal params
      >>
      paramPassing use fps aps

paramPassing  use _fps _aps
    = use

-- ------------------------------------------------------------
