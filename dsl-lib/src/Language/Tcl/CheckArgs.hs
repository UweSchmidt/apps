module Language.Tcl.CheckArgs
where

import Control.Monad
import Control.Applicative              ( )

import Data.Char                        ( isDigit )

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.Expr.Parser         ( parseTclInteger )

-- ------------------------------------------------------------
{-
type CheckArg a
    = String -> Either String a
-}
-- ------------------------------------------------------------

checkArg :: String -> (a -> Maybe b) -> a -> TclEval e s b
checkArg msg check = maybe (tclThrowError msg) return . check

checkBoolean :: (a -> Maybe Bool) -> String -> a -> TclEval e s Bool
checkBoolean c s v = checkArg ("expected boolean value but got " ++ show s) c v

checkBooleanString :: String -> TclEval e s Bool
checkBooleanString s = checkBoolean s2b s s

checkBooleanValue :: Value -> TclEval e s Bool
checkBooleanValue v = checkBoolean v2b (v2s v) v

checkInteger :: (a -> Maybe Integer) -> String -> a -> TclEval e s Integer
checkInteger c s v = checkArg ("expected integer but got " ++ show s) c v

checkIntegerString :: String -> TclEval e s Integer
checkIntegerString s = checkInteger (either (const Nothing) Just . parseTclInteger) s s

checkIntegerValue :: Value -> TclEval e s Integer
checkIntegerValue v0 = checkInteger check (v2s v0) v0
    where
      check v
          | isI v     = selI v
          | isS v     = selS v >>= (either (const Nothing) Just . parseTclInteger)
          | otherwise = mzero

-- ------------------------------------------------------------

toInt :: String -> Maybe Int
toInt s
    | not (null s) && all isDigit s
        = Just $ read s
    | otherwise
        = Nothing

-- ------------------------------------------------------------

tclOption1 :: String -> a -> a -> [Value] -> TclEval e s (a, [Value])
tclOption1 n _d v (x : xs)
    | n == v2s x
	= return (v, xs)

tclOption1 _ d _ xs
    = return (d, xs)

tclOption2 :: String -> a -> (Value -> TclEval e s a) -> [Value] -> TclEval e s (a, [Value])
tclOption2 n _d check (x1 : x2 : xs)
    | n == v2s x1
        = do v <- check x2
             return (v, xs)

tclOption2 _ d _ xs
    = return (d, xs)

-- ------------------------------------------------------------
