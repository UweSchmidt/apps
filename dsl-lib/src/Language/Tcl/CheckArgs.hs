module Language.Tcl.CheckArgs
where

import Control.Monad.Error

import Data.Char                        ( isDigit )

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.Expr.Parser         ( parseTclInteger )

-- ------------------------------------------------------------

type CheckArg a
    = String -> Either String a

-- ------------------------------------------------------------

checkArg :: String -> (a -> Maybe b) -> a -> TclEval e s b
checkArg msg check = maybe (tclThrowError msg) return . check

checkBoolean :: (Show a) => (a -> Maybe Bool) -> a -> TclEval e s Bool
checkBoolean c v = checkArg ("expected boolean value but got " ++ show v) c v

checkBooleanString :: String -> TclEval e s Bool
checkBooleanString = checkBoolean s2b 

checkBooleanValue :: Value -> TclEval e s Bool
checkBooleanValue = checkBoolean v2b 

checkInteger :: (Show a) => (a -> Maybe Integer) -> a -> TclEval e s Integer
checkInteger c v = checkArg ("expected integer but got" ++ show v) c v

checkIntegerString :: String -> TclEval e s Integer
checkIntegerString = checkInteger (either (const Nothing) Just . parseTclInteger)

-- ------------------------------------------------------------

toInt :: String -> Maybe Int
toInt s
    | not (null s) && all isDigit s
        = Just $ read s
    | otherwise
        = Nothing

-- ------------------------------------------------------------

tclOption :: String -> [String] -> (Bool, [String])
tclOption n (x : xs)
    | n == x
	= (True, xs)

tclOption _ xs
    = (False, xs)


tclOption1 :: String -> a -> a -> [String] -> TclEval e s (a, [String])
tclOption1 n _d v (x : xs)
    | n == x
	= return (v, xs)

tclOption1 _ d _ xs
    = return (d, xs)

tclOption2 :: String -> a -> (String -> TclEval e s a) -> [String] -> TclEval e s (a, [String])
tclOption2 n _d check (x1 : x2 : xs)
    | n == x1
        = do v <- check x2
             return (v, xs)

tclOption2 _ d _ xs
    = return (d, xs)

-- ------------------------------------------------------------
