module Language.Tcl.CheckArgs
where

import Control.Monad.Error

import Data.Char                        ( isDigit )

import Language.Tcl.Core
import Language.Tcl.Expr.Parser         ( parseTclInteger )

-- ------------------------------------------------------------

type CheckArg a
    = String -> Either String a

-- ------------------------------------------------------------

tclCheckArg :: CheckArg a -> String -> TclEval e s a
tclCheckArg f x
    = either tclThrowError return $ f x

tclCheckBooleanArg :: String -> TclEval e s Bool
tclCheckBooleanArg
    = tclCheckArg tclBooleanVal

tclCheckIntegerArg :: String -> TclEval e s Integer
tclCheckIntegerArg
    = tclCheckArg tclIntegerVal

-- ------------------------------------------------------------

tclBooleanVal :: CheckArg Bool
tclBooleanVal v
    = maybe ( Left $
              "expected boolean value but got"
              ++ show v
            )
            Right
            ( lookup v (zip ["true", "false", "yes", "no"] [True, False, True, False])
              `mplus`
              fmap (/= 0) (toInt v)
            )

-- ------------------------------------------------------------

tclIntegerVal :: CheckArg Integer
tclIntegerVal v
    = either ( const $
               Left $
               "expected integer but got " ++ show v
             )
            Right
      . parseTclInteger $ v

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

tclOption2 :: String -> a -> CheckArg a -> [String] -> TclEval e s (a, [String])
tclOption2 n _d f (x1 : x2 : xs)
    | n == x1
        = do v <- tclCheckArg f x2
             return (v, xs)

tclOption2 _ d _ xs
    = return (d, xs)

-- ------------------------------------------------------------
