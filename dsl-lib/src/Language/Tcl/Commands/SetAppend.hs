module Language.Tcl.Commands.SetAppend
    ( tclAppend
    , tclSet
    )
where

import Control.Monad.Error
import Control.Monad.RWS

import Language.Tcl.Core
import Language.Tcl.Value

-- ------------------------------------------------------------

tclAppend :: TclCommand e s
tclAppend (var' : values)
    = do val <- lookupVar var
                `mplus`
                return mempty
         setVar var (val `mappend` mconcat values)
    where
      var = selS var'

tclAppend _
    = tclWrongArgs "append varName ?value value value ...?"

-- ------------------------------------------------------------

tclSet	:: TclCommand e s
tclSet [n]
    = lookupVar (selS n)

tclSet [n, v]
    = setVar (selS n) v

tclSet _
    = tclWrongArgs "set varName ?newValue?"

-- ------------------------------------------------------------
