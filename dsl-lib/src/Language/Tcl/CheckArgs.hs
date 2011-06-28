module Language.Tcl.CheckArgs
where

import Control.Monad                    ( )
import Control.Applicative              ( )

import Language.Tcl.Core
import Language.Tcl.Value

-- ------------------------------------------------------------

checkArg :: String -> (a -> Maybe b) -> a -> TclEval e s b
checkArg msg check = maybe (tclThrowError msg) return . check

checkBoolean :: (a -> Maybe Bool) -> String -> a -> TclEval e s Bool
checkBoolean c s v = checkArg ("expected boolean value but got " ++ show s) c v

checkBooleanString :: String -> TclEval e s Bool
checkBooleanString s = checkBoolean string2bool s s

checkBooleanValue :: Value -> TclEval e s Bool
checkBooleanValue v = checkBoolean selB (selS v) v

checkInteger :: (a -> Maybe Integer) -> String -> a -> TclEval e s Integer
checkInteger c s v = checkArg ("expected integer but got " ++ show s) c v

checkIntegerString :: String -> TclEval e s Integer
checkIntegerString s = checkInteger string2integer s s

checkIntegerValue :: Value -> TclEval e s Integer
checkIntegerValue v0 = checkInteger selI (selS v0) v0

checkList :: (a -> Maybe Values) -> String -> a -> TclEval e s Values
checkList c s v = checkArg ("list expected but got " ++ show s) c v

checkListValue :: Value -> TclEval e s Values
checkListValue v0 = checkList selL (selS v0) v0

-- ------------------------------------------------------------

tclOption1 :: String -> a -> a -> Values -> TclEval e s (a, Values)
tclOption1 n _d v (x : xs)
    | n == selS x
	= return (v, xs)

tclOption1 _ d _ xs
    = return (d, xs)

tclOption2 :: String -> a -> (Value -> TclEval e s a) -> Values -> TclEval e s (a, Values)
tclOption2 n _d check (x1 : x2 : xs)
    | n == selS x1
        = do v <- check x2
             return (v, xs)

tclOption2 _ d _ xs
    = return (d, xs)

-- ------------------------------------------------------------
