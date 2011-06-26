module Language.Tcl.Commands.IfForWhile
    ( tclFor
    , tclForeach
    , tclIf
    , tclWhile
    )
where

import Control.Monad.RWS

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs
import Language.Tcl.Expr.Eval           ( evalTclExpr
                                        , substAndEvalTclExpr
                                        )

-- ------------------------------------------------------------

tclForeach :: TclCommand e s
tclForeach [v, l, b]
    = do xs <- evalTclList list
         cs <- parseTclProg body
         _  <- runForeach xs cs
         return mempty
    where
      var  = v2s v
      list = v2s l
      body = v2s b
      runForeach xs cs
          = tclCatchBreakExc $
              mapM_ oneStep xs
              >> return mempty
          where
            oneStep val
                = ( get >>= setVar var val )
                  >> 
                  ( tclCatchContinueExc $
                      evalTclProg cs )

tclForeach _
    = tclWrongArgs "foreach varname list body"

-- ------------------------------------------------------------

tclIf :: TclCommand e s
tclIf (c : kw : tp : rest)
    | v2s kw == "then"
        = tclIf' c tp rest

tclIf (c : tp : rest)
    = tclIf' c tp rest

tclIf _
    = tclWrongArgs "if expr1 ?then? body1 elseif expr2 ?then? body2 elseif ... ?else? ?bodyN?"


tclIf' :: Value -> Value -> TclCommand e s
tclIf' cond thn els
    = do b <- substAndEvalTclExpr (v2s cond) >>= checkBooleanValue
         if b
            then interpreteTcl (v2s thn)
            else tclElse       els

tclElse :: TclCommand e s
tclElse (kw : rest)
    | v2s kw == "elseif"
        = tclIf rest
    | v2s kw == "else"
        = tclElse' rest

tclElse els
    = tclElse' els

tclElse' :: TclCommand e s
tclElse' []
    = return mempty
tclElse' [els]
    = interpreteTcl (v2s els)
tclElse' _
    = tclIf []

-- ------------------------------------------------------------

tclWhile :: TclCommand e s
tclWhile (c' : b' : [])
    = do cond <- parseTclArgs (v2s c')		-- cond and body are parsed only once
         body <- parseTclProg (v2s b')
         tclCatchBreakExc $
           whileLoop cond body
    where
      whileLoop cond body
          = do b <- substTclArgs cond		-- subst args in every loop test
                    >>= evalTclExpr		-- and eval to a boolean
                    >>= checkBooleanValue
               if b
                  then ( tclCatchContinueExc $
                           evalTclProg body )
                       >>
                       whileLoop cond body
                  else return mempty

tclWhile _
    = tclWrongArgs "while test command"

-- ------------------------------------------------------------

tclFor :: TclCommand e s
tclFor (s' : t' : n' : b' : [])
    = do test <- parseTclArgs (v2s t')		-- cond and body are parsed only once
         next <- parseTclProg (v2s n')
         body <- parseTclProg (v2s b')
         interpreteTcl (v2s s')
         tclCatchBreakExc $
           forLoop test next body
    where
      forLoop test next body
          = do b <- substTclArgs test		-- subst args in every loop test
                    >>= evalTclExpr		-- and eval to a boolean
                    >>= checkBooleanValue
               if b
                  then ( tclCatchContinueExc $
                           evalTclProg body )
                       >>
                       evalTclProg next
                       >>
                       forLoop test next body
                  else return mempty

tclFor _
    = tclWrongArgs "for start test next body"

-- ------------------------------------------------------------
