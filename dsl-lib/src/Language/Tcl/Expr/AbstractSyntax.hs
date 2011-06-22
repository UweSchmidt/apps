module Language.Tcl.Expr.AbstractSyntax
where

import Language.Tcl.Value

-- ------------------------------------------------------------

data TclExpr
    = TConst Value
    | TExpr String [TclExpr]
      deriving (Show)

-- ------------------------------------------------------------
