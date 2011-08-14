module Language.Tcl.Commands.Array
    ( tclArray
    )
where

import Control.Applicative              ( (<$>) )

import Control.Monad.RWS                ( mplus )

-- import Data.List                        ( intercalate )
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
        = arrayExists name args
    | cmd == "get"
        = arrayGet name args
    | cmd == "set"
        = arraySet name args
    | cmd == "size"
        = arraySize name args
    | otherwise
        = tclThrowError
          $ "array option must be one of "
                ++ unwords (map show ["exists", "get", "set", "size"])
    where
      cmd  = selS cmd'
      name = selS name'

tclArray _
    = tclWrongArgs "array option arrayName ?arg arg ...?"

arrayExists :: String -> TclCommand e s
arrayExists an []
    = b2i <$> varName isAVN (an, Nothing)

arrayExists _ _
    = tclWrongArgs "array exists arrayName"

arrayGet :: String -> TclCommand e s
arrayGet an []
    = arrayGet an [mkS "*"]

arrayGet an [p]
    = ( lookupArrayVar (an, Nothing)
        >>= return
                . mkL
                . concatMap (\ (k, v) -> [mkS k, v])
                . filter (matchGlobPattern (selS p) . fst)
                . M.toList
      )
      `mplus`
      return lempty

arrayGet _ _
    = tclWrongArgs "array get arrayName ?pattern?"

arraySet :: String -> TclCommand e s
arraySet an [l]
    = do vs <- checkListValue l
         createArray (an, Nothing)
         setVals vs >> return mempty
    where
      setVals []
          = return ()
      setVals (i : v : r)
          = setVar (an, Just . selS $ i) v >> setVals r
      setVals _
          = tclThrowError "list must have an even number of elements"

arraySet _ _
    = tclWrongArgs "array set arrayName list"

arraySize :: String -> TclCommand e s
arraySize an []
    = ( lookupArrayVar (an, Nothing)
        >>= return . mkI . toInteger . M.size
      )
      `mplus`
      return (mkI 0)

arraySize _ _
    = tclWrongArgs "array size arrayName"

-- ------------------------------------------------------------
