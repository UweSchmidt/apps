{- {-# OPTIONS -XGeneralizedNewtypeDeriving #-} -}

module Language.Common.Eval
where

import Control.Exception       	( SomeException, try )
import Control.Monad.Error
import Control.Monad.RWS.Lazy

{-
import Control.Monad            ( )
import Control.Monad.Reader
import Control.Monad.State.Lazy
-}

-- ------------------------------------------------------------
--
-- | Evaluation of an expression/command runs in an
-- error-reader-state-writer-IO monad

type Eval err env wrt st
    = ErrorT err (RWST env wrt st IO)

runEval :: (Error err) => Eval err env wrt st res -> env -> st -> IO (Either err res, st, wrt)
runEval expr env st 
    = runRWST (runErrorT expr) env st

liftIOE	:: (Error err, Monoid wrt) => IO res -> Eval err env wrt st res
liftIOE a
    = do r <- liftIO $ try' a
         case r of
           Left exc
               -> throwError $ strMsg $ show exc
           Right res
               -> return res
    where
      try' :: IO a -> IO (Either SomeException a)
      try' = try

-- ------------------------------------------------------------
