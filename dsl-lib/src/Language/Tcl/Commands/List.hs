module Language.Tcl.Commands.List
    ( tclJoin
    , tclList
    , tclLappend
    , tclLindex
    , tclLinsert
    , tclLlength
    , tclLrange
    , tclLreplace
    , tclSplit
    )
where

import Control.Applicative              ( (<$>) )

import Control.Monad.RWS                ( mplus )

import Data.List                        ( intercalate )

import Language.Tcl.CheckArgs
import Language.Tcl.Core
import Language.Tcl.Expr.Eval           ( evalTclListIndex )
import Language.Tcl.Value

-- ------------------------------------------------------------

tclJoin :: TclCommand e s
tclJoin (l' : s' : [])
    = do vs <- checkListValue l'
         return . mkS . intercalate (selS s') . map selS $ vs

tclJoin (l' : [])
    = tclJoin (l' : mkS " " : [])

tclJoin _
    = tclWrongArgs "join list ?joinString?"

-- ------------------------------------------------------------

tclList :: TclCommand e s
tclList
    = return . mkL

-- ------------------------------------------------------------

tclLappend :: TclCommand e s
tclLappend (var' : values)
    = do val <- lookupVar var
                `mplus`
                return mempty
         list <- lconcat values
         res  <- lappend val list
         setVar var res
    where
      var = selS var'

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

tclSplit :: TclCommand e s
tclSplit (s0 : chars0 : [])
    = return . mkL . map mkS . split $ s
    where
      s     = selS s0
      chars = selS chars0
      split
          | null chars = map (:[])
          | otherwise  = words' (`elem` chars)
      words' _ []
          = []
      words' p s'
          = w1 : words'' ws
            where
              (w1, ws) = break p s'
              words'' []
                  = []
              words'' (_ : [])
                  = "" : []
              words'' (_ : s'')
                  = words' p s''

tclSplit (s' : [])
    = tclSplit [s', mkS " \t\n"]

tclSplit _
    = tclWrongArgs "split string ?splitChars?"

-- ------------------------------------------------------------
