module TclTest
where

import Language.Common.Eval

import Language.Tcl.AbstractSyntax
import Language.Tcl.Parser
import Language.Tcl.Show
import Language.Tcl.Eval

import Text.Parsec

-- ------------------------------------------------------------

p ps s = parse (eofP ps) "" s


type TestEval
    = TclEval () ()

-- ------------------------------------------------------------

execTcl	:: String -> IO ()
execTcl s
    = do (r, st, _w) <- runEval (interpreteTcl s) (initTclEnv ()) (initTclState ())
         putStrLn (show r)
         putStrLn (show . _tvars $ st)


