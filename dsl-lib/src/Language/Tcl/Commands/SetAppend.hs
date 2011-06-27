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
tclAppend (var : values)
    = do val <- (get >>= lookupVar varName)
                `mplus`
                return mempty
         get >>= setVar varName (val `mappend` mconcat values)
    where
      varName = selS var

tclAppend _
    = tclWrongArgs "append varName ?value value value ...?"

-- ------------------------------------------------------------

tclSet	:: TclCommand e s
tclSet [n]
    = get >>= lookupVar (selS n)

tclSet [n, v]
    = get >>= setVar (selS n) v

tclSet _
    = tclWrongArgs "set varName ?newValue?"

-- ------------------------------------------------------------
