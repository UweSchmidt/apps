{-# OPTIONS -XTypeSynonymInstances  -XFlexibleContexts -XFlexibleInstances -XMultiParamTypeClasses #-}

module TclTest
where

import Control.Arrow (first, second, (&&&), (***))
import Control.Applicative ((<$>))
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import Language.Common.Eval

import Language.Tcl.AbstractSyntax
import qualified Language.Tcl.Parser as P
import Language.Tcl.Show
import Language.Tcl.Eval
import Language.Tcl.Value

-- import Text.Parsec

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
{-
type EvalOption os v m = StateT (os, [v]) m

type TclOption os e s = EvalOption os Value (TclEval e s)

type TestOption = EvalOption [String] String Maybe

-- evalOptions :: os -> [v] -> EvalOption os v m a -> m (a, (os, [v]))
-- evalOptions :: t -> t1 -> State (t, t1) a -> (a, (t, t1))
-- evalOptions :: t -> t1 -> StateT (t, t1) m a -> m (a, (t, t1))
-- evalOptions :: Monad m => t -> t1 -> StateT (t, t1) m a -> m (t, t1)

evalOptions :: (Monad m) => os -> [v] -> EvalOption os v m a -> m (os, [v])
evalOptions os vs eval = execStateT eval (os, vs)

arg0 :: (Functor m, Monad m, MonadPlus m, MonadState (os, [v]) m) => EvalOption os v m v
arg0 = do vs <- snd <$> get
          if null vs
             then mzero
             else return (head vs)

modifyOS :: (Monad m) => (os -> os) -> EvalOption os v m ()
modifyOS osf
    = modify (first osf)

evalOpt0 :: (Functor m, Monad m, MonadPlus m, MonadState (os, [v]) m) =>
            (v -> Bool) -> (os -> os) -> EvalOption os v m ()
evalOpt0 name meaning
    = do a <- arg0
         if name a
            then modify (meaning *** tail)
            else return ()

-- instance MonadState ([String], [String]) Maybe

t1 :: Maybe ([String],[String])
t1 = evalOptions [] ["-abc"] (evalOpt0 (== "-abc") ("abc":))

 
-- instance (MonadPlus m) => MonadPlus (EvalOption s o m)

-- runEvalOption :: EvalO
  -}
{-
tclOpt1 :: (MonadPlus m) => Values -> m (a, Values)
tclOpt1 f vs
    = case f

n v (x : xs)
    | n == selS x
	= return (v, xs)
    | otherwise
        = mzero
-}
-- ------------------------------------------------------------
