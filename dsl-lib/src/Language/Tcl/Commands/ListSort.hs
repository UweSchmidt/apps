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
            [ isOpt     (== (mkS "-increasing")) (first $ const increasing)
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

evalCompareCmd :: Value -> TclEval s e (Value -> Value -> TclEval s e Ordering)
evalCompareCmd cv
    = do cf <- undefined cv
         return $ \ x y -> toOrd <$> cf x y
    where
      toOrd i
          | i < 0     = LT
          | i > 0     = GT
          | otherwise = EQ


{-
      lsort ( asc_desc
            , ( (sortby, checkValues)
              , ( caseSensitive
                , ( unique
                  , ()
                  )
                )
              )
            ) (l' : [])
          = do ls <- checkListValue l'
               if null ls || null (tail ls)
                  then return $ mkL ls
                  else do checkValues ls
                          rs <- sortBy cmpFct ls
                          return $ mkL rs
          where
            cmpFct = (asc_desc .) . sortby caseSensitive
            sortBy = sortByM' True unique
-}
{-
sortDefaults :: ( Ordering -> Ordering
                , ( ( (String -> String) -> Value -> Value -> Ordering
                    , Values -> TclEval e s ()
                    )
                  , (a -> a, (Bool, ()))
                  )
                )
sortDefaults = ( increasing
               , ( sortByAscii
                 , ( id
                   , ( False
                     , ()
                     )
                   )
                 )
               )

type SortOpt m = (CmpFct m, ())
type SetSortOpt m = SortOpt m -> SortOpt m

type CmpFct m = m Ordering -> m Ordering

-- sortDefaults :: SortOpt (TclEval e s)
sortDefaults = (increasing, ())

-- increasing :: (Monad m) => m Ordering -> m Ordering
increasing  = id

-- decreasing :: (Monad m) => m Ordering -> m Ordering
decreasing = (cmpl <$>)
    where
      cmpl LT = GT
      cmpl GT = LT
      cmpl x  = x



nocase :: String -> String
nocase = map toLower

sortByAscii   :: (Monad m) => ((String -> String) -> Value -> Value -> m Ordering, Values -> TclEval e s ())
sortByAscii   = (\ conv -> compare `on` (conv . selS), const $ return () )

sortByInteger :: (Monad m) => ((String -> String) -> Value -> Value -> m Ordering, Values -> TclEval e s ())
sortByInteger = (\ _ -> compare `on` (fromJust . selI), mapM_ checkIntegerValue)

sortByReal :: (Monad m) => ((String -> String) -> Value -> Value -> m Ordering, Values -> TclEval e s ())
sortByReal = (\ _ -> compare `on` (fromJust . selI), mapM_ checkDoubleValue)

-- not yet all Tcl 8.5 sort variants implemented,
-- this requires a monadic version of sortBy,
-- due to the arbitray compare functions given in the -command option

-- sortOptions :: OptParser [Value] ((a -> Ordering) -> a -> Ordering, ((Value -> Value -> Ordering, Values -> TclEval e s ()), d))
sortOptions :: OptParser [Value] ( Ordering -> Ordering
                                 , ( ( (String -> String) -> Value -> Value -> Ordering
                                     , Values -> TclEval e s ())
                                   , ( String -> String
                                     , (Bool, d)
                                     )
                                   )
                                 )

sortOptions :: OptParser [Value] (SortOpt (TclEval e s))
sortOptions
    = options
      [ isOpt (== (mkS "-increasing")) (first $ const increasing)
      , isOpt (== (mkS "-decreasing")) (first $ const decreasing)
      , isOpt (== (mkS "-ascii"     )) id -- (second . first $ const sortByAscii)
      , isOpt (== (mkS "-integer"   )) id -- (second . first $ const sortByInteger)
      , isOpt (== (mkS "-real"      )) id -- (second . first $ const sortByReal)
      , isOpt (== (mkS "-nocase"    )) id -- (second . second . first $ const nocase)
      , isOpt (== (mkS "-unique"    )) id -- (second . second . second . first $ const True)
      ]
-}
-- ------------------------------------------------------------
