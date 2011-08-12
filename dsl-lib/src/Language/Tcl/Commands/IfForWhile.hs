module Language.Tcl.Commands.IfForWhile
    ( tclFor
    , tclForeach
    , tclIf
    , tclSwitch
    , tclWhile
    )
where

import Control.Arrow                    ( first, second )
import Control.Monad.RWS

import Data.Char                        ( toLower )
import Data.List                        ( isPrefixOf )
import Data.Function                    ( on )

import Language.Common.EvalOptions

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
      var  = selS v
      list = selS l
      body = selS b
      runForeach xs cs
          = tclCatchBreakExc $
              mapM_ oneStep xs
              >> return mempty
          where
            oneStep val
                = ( setVar var val )
                  >> 
                  ( tclCatchContinueExc $
                      evalTclProg cs )

tclForeach _
    = tclWrongArgs "foreach varname list body"

-- ------------------------------------------------------------

tclIf :: TclCommand e s
tclIf (c : kw : tp : rest)
    | selS kw == "then"
        = tclIf' c tp rest

tclIf (c : tp : rest)
    = tclIf' c tp rest

tclIf _
    = tclWrongArgs "if expr1 ?then? body1 elseif expr2 ?then? body2 elseif ... ?else? ?bodyN?"


tclIf' :: Value -> Value -> TclCommand e s
tclIf' cond thn els
    = do b <- substAndEvalTclExpr (selS cond) >>= checkBooleanValue
         if b
            then interpreteTcl (selS thn)
            else tclElse       els

tclElse :: TclCommand e s
tclElse (kw : rest)
    | selS kw == "elseif"
        = tclIf rest
    | selS kw == "else"
        = tclElse' rest

tclElse els
    = tclElse' els

tclElse' :: TclCommand e s
tclElse' []
    = return mempty
tclElse' [els]
    = interpreteTcl (selS els)
tclElse' _
    = tclIf []

-- ------------------------------------------------------------

tclWhile :: TclCommand e s
tclWhile (c' : b' : [])
    = do cond <- parseTclArgs (selS c')		-- cond and body are parsed only once
         body <- parseTclProg (selS b')
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
    = do test <- parseTclArgs (selS t')		-- cond and body are parsed only once
         next <- parseTclProg (selS n')
         body <- parseTclProg (selS b')
         _ <- interpreteTcl (selS s')
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

tclSwitch :: TclCommand e s
tclSwitch l
    = do let (l1, lr1) = splitAt (length l - 2) l
         ((cmp, cv), l2) <- tclFromEither
                            . evalOptions switchOptions switchOptionDefault $ l1
         tclSwitch1 (cmp `on` cv) (l2 ++ lr1)

tclSwitch1 :: (String -> String -> Bool) -> TclCommand e s
tclSwitch1 cmp (s : pbs : [])
    = do ls <- checkListValue pbs
         tclSwitch2 cmp (s : ls)
tclSwitch1 cmp l
    = tclSwitch2 cmp l

tclSwitch2 :: (String -> String -> Bool) -> TclCommand e s
tclSwitch2 cmp (s : pbs)
    | l < 2 || l `mod` 2 == 1
        = tclSwitchWrongArgs
    | otherwise
        = let (p1 : b1 : pbs1) = pbs in
          tclSwitch3 cmp (selS s) p1 b1 pbs1 
    where
      l = length pbs

tclSwitch2 _ _
    = tclSwitchWrongArgs

tclSwitch3 :: (String -> String -> Bool) -> String -> Value -> Value -> TclCommand e s
tclSwitch3 cmp s p1 b1 pbs
    | null pbs
        = if ( not emptyBody
               &&
               ( found
                 ||
                 p1' == "default"
               )
             )
          then interpreteTcl b1'
          else return mempty
    | not found
        = tclSwitch3 cmp s b2 p2 pb2
    | emptyBody
        = tclSwitch3 (const $ const True) s b2 p2 pb2
    | otherwise
        = interpreteTcl b1'
    where
      p1'       = selS p1
      found     = cmp p1' s
      b1'       = selS b1
      emptyBody = take 1 b1' == "-"
      (b2 : p2 : pb2) = pbs

tclSwitchWrongArgs :: TclEval e s r
tclSwitchWrongArgs
    = tclWrongArgs $
      unlines [ "switch ?options? string pattern body ?pattern body ...?"
              , "or"
              , "switch ?options? string {pattern body ?pattern body ...?}"
              ]

switchOptions :: OptParser [Value] SwitchOptions
switchOptions
    = optionsUntil (isOpt ((== "--") . selS) id)
      [ isOpt        ((== "-exact"     ) . selS) (first  $ const (==))
      , isOpt        ((== "-glob"      ) . selS) (first  $ const matchGlobPattern)
      , isOpt        ((== "-nocase"    ) . selS) (second $ const $ map toLower)
      , isIllegalOpt (("-" `isPrefixOf`) . selS)
      ]

type SwitchOptions = (String -> String -> Bool, String -> String)

switchOptionDefault :: SwitchOptions
switchOptionDefault = ((==), id)

-- ------------------------------------------------------------
