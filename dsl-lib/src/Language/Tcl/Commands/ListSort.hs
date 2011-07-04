module Language.Tcl.Commands.ListSort
    ( tclLsort
    )
where

-- import Control.Applicative              ( (<$>) )
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
      lsort () ([l])
          = undefined
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
      lsort _so _
          = tclWrongArgs "lsort ?options? list"
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
-}
type SortOpt = ()
type SetSortOpt = SortOpt -> SortOpt

sortDefaults :: SortOpt
sortDefaults = ()
{-
nocase :: String -> String
nocase = map toLower

sortByAscii   :: (Monad m) => ((String -> String) -> Value -> Value -> m Ordering, Values -> TclEval e s ())
sortByAscii   = (\ conv -> compare `on` (conv . selS), const $ return () )

sortByInteger :: (Monad m) => ((String -> String) -> Value -> Value -> m Ordering, Values -> TclEval e s ())
sortByInteger = (\ _ -> compare `on` (fromJust . selI), mapM_ checkIntegerValue)

sortByReal :: (Monad m) => ((String -> String) -> Value -> Value -> m Ordering, Values -> TclEval e s ())
sortByReal = (\ _ -> compare `on` (fromJust . selI), mapM_ checkDoubleValue)

increasing :: (Monad m) => m Ordering -> m Ordering
increasing  = id

decreasing :: (Monad m) => m Ordering -> m Ordering
decreasing = (cmpl <$>)
    where
      cmpl LT = GT
      cmpl GT = LT
      cmpl x  = x

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
-}

sortOptions :: OptParser [Value] SortOpt
sortOptions
    = options
      [ isOpt (== (mkS "-increasing")) id -- (first $ const increasing)
      , isOpt (== (mkS "-decreasing")) id -- (first $ const decreasing)
      , isOpt (== (mkS "-ascii"     )) id -- (second . first $ const sortByAscii)
      , isOpt (== (mkS "-integer"   )) id -- (second . first $ const sortByInteger)
      , isOpt (== (mkS "-real"      )) id -- (second . first $ const sortByReal)
      , isOpt (== (mkS "-nocase"    )) id -- (second . second . first $ const nocase)
      , isOpt (== (mkS "-unique"    )) id -- (second . second . second . first $ const True)
      ]

-- ------------------------------------------------------------
