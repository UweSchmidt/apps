module Language.Tcl.Commands.Array
    ( tclArray
    )
where

import Control.Applicative              ( (<$>) )

import Control.Monad.RWS                ( mplus )

import Data.List                        ( intercalate )
import qualified
       Data.Map                         as M
import Language.Tcl.CheckArgs
import Language.Tcl.Core
-- import Language.Tcl.Expr.Eval           ( evalTclListIndex )
import Language.Tcl.Value

-- ------------------------------------------------------------

tclArray :: TclCommand e s
tclArray (cmd' : name' : args)
    | cmd == "exists"
        = arrayExists an args
    | cmd == "get"
        = arrayGet    an args'
    | cmd == "names"
        = arrayNames  an args'
    | cmd == "set"
        = arraySet    an args
    | cmd == "size"
        = arraySize   an args
    | cmd == "unset"
        = arrayUnset  an args'
    | otherwise
        = tclThrowError
          $ "array option must be one of "
                ++ ( intercalate ", " . map show
                     $ ["exists", "get", "set", "size"]
                   )
    where
      cmd   = selS cmd'
      name  = selS name'
      an    = (name, Nothing)
      args' = map selS args

tclArray _
    = tclWrongArgs "array option arrayName ?arg arg ...?"

-- ------------------------------------------------------------

arrayExists :: TclVarName -> TclCommand e s
arrayExists an []
    = b2i <$> varName isAVN an

arrayExists _ _
    = tclWrongArgs "array exists arrayName"

-- ------------------------------------------------------------

arrayGet :: TclVarName -> [String] -> TclEval e s Value
arrayGet an []
    = arrayGet an ["*"]

arrayGet an [p]
    = ( lookupArrayVar an
        >>= return
                . mkL
                . concatMap (\ (k, v) -> [mkS k, v])
                . ( if p == "*"
                    then id
                    else filter (matchGlobPattern p . fst)
                  )
                . M.toList
      )
      `mplus`
      return lempty

arrayGet _ _
    = tclWrongArgs "array get arrayName ?pattern?"

-- ------------------------------------------------------------

arrayNames :: TclVarName -> [String] -> TclEval e s Value
arrayNames an []
    = arrayNames' id an

arrayNames an (m : p : [])
    | m == "-exact"
        = arrayNames' (filter (== p)) an
    | m == "-glob"
        = arrayNames' (filter (matchGlobPattern p)) an
    | otherwise
        = tclThrowError $ "bad option " ++ show m ++ ": must be -exact or -glob"

arrayNames an [p]
    = arrayNames' (filter (matchGlobPattern p)) an

arrayNames _ _
    = tclWrongArgs "array get arrayName ?mode? ?pattern?"

arrayNames' :: ([String] -> [String]) -> TclVarName -> TclEval e s Value
arrayNames' filter' an
    = ( lookupArrayVar an
        >>= return
                . mkL
                . map mkS
                . filter'
                . M.keys
      )
      `mplus`
      return lempty

-- ------------------------------------------------------------

arraySet :: TclVarName -> TclCommand e s
arraySet an [l]
    = do vs <- checkListValue l
         createArray an
         setVals vs >> return mempty
    where
      setVals []
          = return ()
      setVals (i : v : r)
          = setVar (fst an, Just . selS $ i) v >> setVals r
      setVals _
          = tclThrowError "list must have an even number of elements"

arraySet _ _
    = tclWrongArgs "array set arrayName list"

-- ------------------------------------------------------------

arraySize :: TclVarName -> TclCommand e s
arraySize an []
    = ( lookupArrayVar an
        >>= return . mkI . toInteger . M.size
      )
      `mplus`
      return (mkI 0)

arraySize _ _
    = tclWrongArgs "array size arrayName"

-- ------------------------------------------------------------

arrayUnset :: TclVarName -> [String] -> TclEval e s Value
arrayUnset an [p]
    = lookupArrayVar an
      >>= return    . filter (matchGlobPattern p) . M.keys
      >>= sequence_ . map (\ i -> unsetVar (fst an, Just i))
      >>  return mempty

arrayUnset vn []
    = ( lookupArrayVar vn	-- check whether an denotes an array
        >> unsetVar vn          -- remove the whole array
        >> return mempty
      )
      `mplus`
      return mempty

arrayUnset _ _
    = tclWrongArgs "array unset arrayName ?pattern?"

-- ------------------------------------------------------------
