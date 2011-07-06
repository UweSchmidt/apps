module Language.Tcl.Commands.Info
    ( tclInfo
    )
where

import Control.Applicative              ( (<$>) )
import Control.Monad.RWS

import Data.Maybe                       ( fromJust )
import Data.List                        ( sort )

import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value
-- import Language.Tcl.CheckArgs
-- import Language.Tcl.Expr.Eval           ( evalTclListIndex )

import Text.Parsec                      ( (<|>)
                                        , setState
                                        )

-- ------------------------------------------------------------

tclInfo :: TclCommand e s
tclInfo l0
    = do (subCmd, _l) <- tclFromEither . evalOptions infoSubCmd (tclThrowError "mist") $ l0
         subCmd
    where
      vars = [ ("vars",     varNames      )
             , ("globals",  globalVarNames)
             , ("locals",   localVarNames )
             , ("procs",    procNames     )
             , ("commands", commandNames  )
             ]

      infoSubCmd
          = ( do c <- nextArgWith (`elem` map (mkS . fst) vars)
                 p <- singleOptArg (mkS "*") ("wrong # args: should be \"info " ++ selS c ++ " ?pattern?\"")
                 setState $ infoVars (fromJust . lookup (selS c) $ vars) p
            )
            <|>
            ( do _ <- nextArgWith (== mkS "exists")
                 n <- singleArg "wrong # args: should be \"info exists varName\""
                 setState $ infoExists n
            )
            <|>
            ( do c <- nextArg
                 illegalArgs $ "unknown subcommand " ++ show (selS c) ++ " must be commands, exists, globals, locals, procs, vars"
            )
            <|>
            illegalArgs "wrong # args: should be \"info subcommand ?argument ...?\""

infoExists :: Value -> TclEval e s Value
infoExists n
    = b2i
      <$> (get >>= varName (selS n))

infoVars :: (TclState e s -> TclEval e s [String]) -> Value -> TclEval e s Value
infoVars names pat
    = mkL . map mkS . sort . filter (matchPattern $ selS pat)
      <$> (get >>= names)

matchPattern :: String -> String -> Bool
matchPattern pat = const True		-- TODO implement that in string match

{-
            , isOpt     (== (mkS "-decreasing")) (first $ const decreasing)
            , isOpt     (== (mkS "-unique"    )) (second . first $ const True)
            , isOpt     (== (mkS "-indices"   )) (second . second . first $ const True)
            , isOpt     (== (mkS "-ascii"     )) (second . second . second . first $ const (conv2ascii,   compareS))
            , isOpt     (== (mkS "-integer"   )) (second . second . second . first $ const (conv2integer, compareI))
            , isOpt     (== (mkS "-real"      )) (second . second . second . first $ const (conv2double,  compareD))
            , isOpt     (== (mkS "-nocase"    )) (second . second . second . second . first $ const notCaseSensitive)
            , isArgOpt  (== (mkS "-command"   )) (\ v ->
                                                  second . second . second . first $ const (noconv,       compareC v))
            ]
-}

-- ------------------------------------------------------------
