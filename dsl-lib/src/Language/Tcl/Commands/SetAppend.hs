module Language.Tcl.Commands.SetAppend
    ( tclAppend
    , tclConcat
    , tclEval
    , tclSet
    , tclUnset
    )
where

import Control.Monad.Error
import Control.Monad.RWS

import Data.List                        ( isPrefixOf )

import Language.Common.EvalOptions

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

tclConcat :: TclCommand e s
tclConcat
    = return
      . mkS
      . unwords
      . map (trimWhiteSpace . selS)

-- ------------------------------------------------------------

tclEval :: TclCommand e s
tclEval l@(_ : _)
    = interpreteTcl
      . unwords
      . map (trimWhiteSpace . selS) $ l

tclEval _
    = tclWrongArgs "eval arg ?arg ...?"

-- ------------------------------------------------------------

tclSet	:: TclCommand e s
tclSet [n]
    = lookupVar (selS n)

tclSet [n, v]
    = setVar (selS n) v

tclSet _
    = tclWrongArgs "set varName ?newValue?"

-- ------------------------------------------------------------

tclUnset :: TclCommand e s
tclUnset l0
    = do (complain, l) <- tclFromEither
                          . evalOptions unsetOptions True $ l0
         sequence_ $ map (tclUnsetVariable complain) l
         return mempty

tclUnsetVariable :: Bool -> Value -> TclEval e s Value
tclUnsetVariable complain var0
    = do ex <- varName var
         if not ex
            then when complain $ tclThrowError $ "can't unset " ++ show var ++ ": no such variable"
            else unsetVar var >> return mempty
         return mempty
    where
      var = selS var0

unsetOptions :: OptParser [Value] Bool
unsetOptions
    = optionsUntil (isOpt ((== "--") . selS) id)
      [ isOpt        ((== "-nocomplain") . selS) (const False)
      , isIllegalOpt (("-" `isPrefixOf`) . selS)
      ]

