module Language.Tcl.Commands.List
    ( tclList
    , tclLappend
    , tclLindex
    , tclLinsert
    , tclLlength
    , tclLrange
    , tclLreplace
    , tclLsort
    )
where

import Control.Applicative              ( (<$>) )
import Control.Arrow                    ( first
                                        , second
                                        )
import Control.Monad.RWS

import Data.Char                        ( toLower )
import Data.Function                    ( on )
import Data.Maybe                       ( fromJust )

import Language.Common.EvalOptions

import Language.Tcl.Core
import Language.Tcl.Value
import Language.Tcl.CheckArgs
import Language.Tcl.Expr.Eval           ( evalTclListIndex )

-- ------------------------------------------------------------

tclList :: TclCommand e s
tclList
    = return . mkL

-- ------------------------------------------------------------

tclLappend :: TclCommand e s
tclLappend (var : values)
    = do val <- (get >>= lookupVar varName)
                `mplus`
                return mempty
         list <- lconcat values
         res  <- lappend val list
         get >>= setVar varName res
    where
      varName = selS var

tclLappend _
    = tclWrongArgs "lappend varName ?value value value ...?"

-- ------------------------------------------------------------

tclLindex :: TclCommand e s
tclLindex (list : [])
    = tclLindex (list : mempty : [])
tclLindex (list : indices@(_:_:_))
    = tclLindex (list : mkL indices: [])
tclLindex (list : indexList : [])
    = checkListValue indexList
      >>= lindex list
    where
      lindex l []
          = return l
      lindex l (i : ixs)
          = do l'      <- checkListValue l
               let len = length l'
               i'      <- evalTclListIndex len i
               let res = if i' < 0 || i' >= len
                         then mempty
                         else l' !! i'
               lindex res ixs
                  
tclLindex _
    = tclWrongArgs "lindex list ?index...?"

-- ------------------------------------------------------------

tclLrange :: TclCommand e s
tclLrange (list' : first' : last' : [])
    = do list    <- checkListValue list'
         let len =  length list
         firsT   <- (`max` 0)         <$> evalTclListIndex len first'
         lasT    <- (`min` (len - 1)) <$> evalTclListIndex len last'
         return . mkL . take (lasT - firsT + 1) . drop firsT $ list
                  
tclLrange _
    = tclWrongArgs "lrange list first last"

-- ------------------------------------------------------------

tclLreplace :: TclCommand e s
tclLreplace (list' : first' : last' : elems')
    = do list      <- checkListValue list'
         let elems =  mkL elems'
         let len   =  length list
         if null list
            then return elems
            else do firsT <- ( 0          `max`) <$> evalTclListIndex len first'
                    lasT  <- ((firsT - 1) `max`) <$> evalTclListIndex len last'
                    let (vs1, rest) = splitAt firsT list
                    res1  <- elems `lappend` mkL (drop (lasT - firsT + 1) rest)
                    mkL vs1 `lappend` res1
tclLreplace _
    = tclWrongArgs "lreplace list first last ?element element ...?"

-- ------------------------------------------------------------

tclLinsert :: TclCommand e s
tclLinsert (list' : index' : values@(_ : _))
    = do list    <- checkListValue list'
         let len =  length list
         let val =  mkL values
         index   <- evalTclListIndex len index'
         case index of
           ix | ix <= 0   ->     val  `lappend` mkL list
              | ix >= len -> mkL list `lappend` val
              | otherwise -> let (v1, v2) = splitAt ix list in
                             do rest <- val `lappend` mkL v2
                                mkL v1 `lappend` rest
tclLinsert _
    = tclWrongArgs "linsert list index element ?element element ...?"

-- ------------------------------------------------------------

tclLlength :: TclCommand e s
tclLlength (list : [])
    = checkListValue list
      >>= (return . mkI . toInteger . length)

tclLlength _
    = tclWrongArgs "llength list"

-- ------------------------------------------------------------

tclLsort :: TclCommand e s
tclLsort l0
    = do (so, l) <- tclFromEither . evalOptions sortOptions sortDefaults $ l0
         lsort so l
    where
      lsort ( asc_desc
            , ( ( sortby, checkValues)
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
                          undefined

      lsort _so _
          = tclWrongArgs "lsort ?options? list"

sortDefaults = ( increasing
               , ( sortByAscii
                 , ( id
                   , ( False
                     , ()
                     )
                   )
                 )
               )

nocase :: String -> String
nocase = map toLower

sortByAscii   :: ((String -> String) -> Value -> Value -> Ordering, Values -> TclEval e s ())
sortByAscii   = (\ conv -> compare `on` (conv . selS), const $ return () )

sortByInteger :: ((String -> String) -> Value -> Value -> Ordering, Values -> TclEval e s ())
sortByInteger = (\ _ -> compare `on` (fromJust . selI), mapM_ checkIntegerValue)

sortByReal :: ((String -> String) -> Value -> Value -> Ordering, Values -> TclEval e s ())
sortByReal = (\ _ -> compare `on` (fromJust . selI), mapM_ checkDoubleValue)

increasing :: (a -> Ordering) -> a -> Ordering
increasing  = (id .)

decreasing :: (a -> Ordering) -> a -> Ordering
decreasing = (cmpl .)
    where
      cmpl LT = GT
      cmpl GT = LT
      cmpl x  = x

-- sortOptions :: OptParser [Value] ((a -> Ordering) -> a -> Ordering, ((Value -> Value -> Ordering, Values -> TclEval e s ()), d))
sortOptions
    = options
      [ isOpt (== (mkS "-increasing")) (first $ const increasing)
      , isOpt (== (mkS "-decreasing")) (first $ const decreasing)
      , isOpt (== (mkS "-ascii"     )) (second . first $ const sortByAscii)
      , isOpt (== (mkS "-integer"   )) (second . first $ const sortByInteger)
      , isOpt (== (mkS "-real"      )) (second . first $ const sortByReal)
      , isOpt (== (mkS "-nocase"    )) (second . second . first $ const nocase)
      , isOpt (== (mkS "-unique"    )) (second . second . second . first $ const True)
      ]

-- ------------------------------------------------------------
