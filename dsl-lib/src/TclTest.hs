module TclTest
where

import Control.Monad.Error
import Language.Common.Eval

import Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser as P
import Language.Tcl.Show
import Language.Tcl.Eval

import Text.Parsec

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

