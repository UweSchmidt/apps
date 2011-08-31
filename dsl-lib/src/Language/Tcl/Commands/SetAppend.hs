module Language.Tcl.Commands.SetAppend
    ( tclAppend
    , tclConcat
    , tclEval
    , tclLassign
    , tclSet
    , tclUnset
    )
where

import Control.Monad.Error
import Control.Monad.RWS

import Data.List                        ( isPrefixOf )

import Language.Common.EvalOptions

import Language.Tcl.CheckArgs
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
      var = selVN var'

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

tclLassign :: TclCommand e s
tclLassign (l : vars@(_ : _))
    = do vals <- checkListValue l
         assign vars vals
    where
      assign [] vs
          = return . mkL $ vs
      assign (n : ns) []
          = tclSet [n, mempty] >> assign ns []
      assign (n : ns) (v : vs)
          = tclSet [n, v] >> assign ns vs

tclLassign _
    = tclWrongArgs "lassign list varName ?varName ...?"

-- ------------------------------------------------------------

tclSet	:: TclCommand e s
tclSet [n]
    = lookupVar . selVN $ n

tclSet [n, v]
    = setVar (selVN n) v

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
    = do ex <- varName isVN var
         if not ex
            then when complain $ tclThrowError $ "can't unset " ++ show var ++ ": no such variable"
            else unsetVar var >> return mempty
         return mempty
    where
      var = selVN var0

unsetOptions :: OptParser [Value] Bool
unsetOptions
    = optionsUntil (isOpt ((== "--") . selS) id)
      [ isOpt        ((== "-nocomplain") . selS) (const False)
      , isIllegalOpt (("-" `isPrefixOf`) . selS) "must be -nocomplain or --"
      ]

-- ------------------------------------------------------------

