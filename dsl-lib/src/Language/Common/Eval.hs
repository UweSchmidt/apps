{-# OPTIONS -XGeneralizedNewtypeDeriving #-}

module Language.Common.Eval
where

import Control.Exception       	( SomeException, try )
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy

-- ------------------------------------------------------------
--
-- | Evaluation of an expression/command runs in an
-- error-reader-state-writer-IO monad

type Eval env wrt st
    = ErrorT String (RWST env wrt st IO)

runEval :: Eval env wrt st res -> env -> st -> IO (Either String res, st, wrt)
runEval expr env st 
    = runRWST (runErrorT expr) env st

liftIOE	:: Monoid wrt => IO res -> Eval env wrt st res
liftIOE a
    = do r <- liftIO $ try' a
         case r of
           Left exc
               -> throwError $ show exc
           Right res
               -> return res
    where
      try' :: IO a -> IO (Either SomeException a)
      try' = try

-- ------------------------------------------------------------
