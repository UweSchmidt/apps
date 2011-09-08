module Language.Tcl.Commands.ListSort
    ( tclLsort
    )
where

import Control.Applicative              ( (<$>) )
import Control.Arrow                    ( first
                                        , second
                                        )
import Data.Char                        ( toLower )
import Data.Function                    ( on )
import Data.List.MergeSort              ( sortByM' )

import Data.Maybe                       ( fromJust )

import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs
-- import Language.Tcl.Expr.Eval           ( evalTclListIndex )

-- ------------------------------------------------------------

tclLsort :: TclCommand e s
tclLsort l0
    = do (so, l) <- tclFromEither . evalOptions sortOptions sortDefaults $ l0
         lsort so l
    where
      lsort (incrORdecr, (unique, (indices, ((conv, ccmp), (csSensitive, ()))))) ([l])
          = do ls  <- checkListValue l
               cmp <- ccmp                              -- the compare function is computed in the TclEval monad,
                                                        -- to evaluate the command string to a compare function

               if null ls || null (tail ls)             -- no action for empty or single elem list
                  then return $ mkL ls
                  else do ls' <- mapM conv' ls          -- construct the list on which the compares are performed
                          let ls'' = if indices		-- throw away the original list
                                     then map mkI [0..] -- and take the list of indices
                                     else ls
                          rs' <- map snd <$> sort (cmpFct' cmp) (zip ls' ls'')
                          return $ mkL rs'
          where
            cmpFct' cmp
                = cmpFct `on` fst
                where
                  cmpFct
                      = app2 incrORdecr cmp

            sort   = sortByM' False unique

            conv' = conv csSensitive

            app2 f g = \ x y -> f <$> g x y

      lsort _so _
          = tclWrongArgs "lsort ?options? list"

      noconv _cv v
          = return v

      conv2ascii cv v
          = cv <$> convS v

      conv2integer _cs v
          = mkI <$> checkIntegerValue v

      conv2double _cs v
          = mkD <$> checkDoubleValue v

      increasing
          = id

      decreasing
          = cmpl
          where
            cmpl LT = GT
            cmpl GT = LT
            cmpl x  = x

      compareS
          = return ((return .) . compare `on` selS)

      compareI
          = return ((return .) . compare `on` (fromJust . selI))

      compareD
          = return ((return .) . compare `on` (fromJust . selD))

      compareC cv
          = evalCompareCmd cv

      caseSensitive
          = id

      notCaseSensitive
          = mkS . map toLower . selS

      sortDefaults
          = (increasing, (False, (False, ((conv2ascii, compareS), (caseSensitive, ())))))

      sortOptions
          = options
            [ isOpt     ((== "-increasing") . selS) (first $ const increasing)
            , isOpt     ((== "-decreasing") . selS) (first $ const decreasing)
            , isOpt     ((== "-unique"    ) . selS) (second . first $ const True)
            , isOpt     ((== "-indices"   ) . selS) (second . second . first $ const True)
            , isOpt     ((== "-ascii"     ) . selS) (second . second . second . first $ const (conv2ascii,   compareS))
            , isOpt     ((== "-integer"   ) . selS) (second . second . second . first $ const (conv2integer, compareI))
            , isOpt     ((== "-real"      ) . selS) (second . second . second . first $ const (conv2double,  compareD))
            , isOpt     ((== "-nocase"    ) . selS) (second . second . second . second . first $ const notCaseSensitive)
            , isArgOpt  ((== "-command"   ) . selS) (\ v ->
                                                     second . second . second . first $ const (noconv,       compareC v))
            ]

evalCompareCmd :: Value -> TclEval s e (Value -> Value -> TclEval s e Ordering)
evalCompareCmd cv
    = do vs <- evalTclArgs $ selS cv
         return $ \ x y -> toOrd <$> cf (vs ++ [x, y])
    where
      cf vs 
          = evalTcl vs >>= checkIntegerValue

      toOrd i
          | i < 0     = LT
          | i > 0     = GT
          | otherwise = EQ

-- ------------------------------------------------------------
