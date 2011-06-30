{-# OPTIONS -XTypeSynonymInstances  -XFlexibleContexts -XFlexibleInstances -XMultiParamTypeClasses #-}

module TclTest
where

import Control.Arrow -- (first, second, (&&&), (***))
import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Data.List ( isPrefixOf )

import Language.Common.Eval
import Language.Common.EvalOptions

import Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser as P
import Language.Tcl.Show
import Language.Tcl.Eval
import Language.Tcl.Value

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos

-- ------------------------------------------------------------

type TestEval
    = TclEval () ()

-- ------------------------------------------------------------

execTcl	:: String -> IO ()
execTcl s
    = do (r, st, _w) <- runEval (interpreteTcl s) (initTclEnv ()) (initTclState ())
         putStrLn (show r)
         putStrLn (show st)

testTcl :: Error err => Eval err (TclEnv ()) wrt (TclState e ()) res -> IO (Either err res, TclState e (), wrt)
testTcl s
    = runEval s  (initTclEnv ()) (initTclState ())

-- ------------------------------------------------------------
