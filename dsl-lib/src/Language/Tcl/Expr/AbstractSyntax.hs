module Language.Tcl.Expr.AbstractSyntax
where

-- ------------------------------------------------------------

data TclExpr
    = TIConst Integer
    | TFConst Double
    | TSConst String
    | TExpr String [TclExpr]
      deriving (Show)

-- ------------------------------------------------------------
