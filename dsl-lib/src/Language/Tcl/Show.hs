module Language.Tcl.Show
    ( showTclProg
    , showTclCmd
    , showTclList
    , showTclArgs
    , showTclArg
    , showTclSubst
    )
where

import Data.List	( intercalate )

import Language.Tcl.AbstractSyntax
import Language.Tcl.Value

-- ------------------------------------------------------------

showTclProg :: TclProg -> String
showTclProg
    = intercalate "\n" . map showTclCmd . _tclProg

showTclCmd :: TclCmd -> String
showTclCmd
    = showTclArgs . _tclCmd

showTclList :: TclList -> String
showTclList
    = showTclArgs . _tclList

showTclArgs :: [TclArg] -> String
showTclArgs
    = intercalate " " . map showTclArg 

showTclArg :: TclArg -> String
showTclArg (TclArg a)
    = stringToList . concatMap showTclSubst $ a

showTclSubst :: TclSubst -> String
showTclSubst (TLit s)
    = s
showTclSubst (TVar n)
    = "$" ++ n
showTclSubst (TEval pg)
    = "[" ++ showTclProg pg ++ "]"

-- ------------------------------------------------------------
