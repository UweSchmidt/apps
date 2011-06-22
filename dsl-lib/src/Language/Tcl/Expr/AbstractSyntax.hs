module Language.Tcl.Expr.AbstractSyntax
where

import Language.Tcl.Value

-- ------------------------------------------------------------

data TclExpr
    = TConst Value
    | TExpr String [TclExpr]
      deriving (Show)

{-
instance Show TclExpr where
    show (TConst v)    = v2s v
    show (TExpr op al) = "(" ++ op ++ concatMap show al ++ ")"
-}

-- ------------------------------------------------------------
