{-# OPTIONS -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XTypeSynonymInstances #-}

-- ------------------------------------------------------------

module Control.Monad.ReaderStateIOError
    ( module Control.Monad.ReaderStateIOError
    , mzero, mplus
    , liftIO
    , ask, asks, local
    , get, gets, put, modify
    , throwError, catchError
    )
where

import          Control.Applicative
import          Control.Monad
import          Control.Monad.Error
import          Control.Monad.Reader
import          Control.Monad.State

-- ------------------------------------------------------------

-- |
-- reader state io monad implemented directly without any monad transformers

newtype ReaderStateIOError env state res
    = RSIOE { runReaderStateIOError :: env -> state -> IO (Either String res, state) }

instance
    Monad (ReaderStateIOError env state)
    where
    return v
        = RSIOE $
          \ _e s -> return (Right v, s)

    fail msg
        = RSIOE $
          \ _e s -> return (Left $ "fail: " ++ msg, s)

    RSIOE cmd >>= f
        = RSIOE $
          \ e s -> do
                   (r', s') <- cmd e s
                   case r' of
                     Left  e' -> return (Left e', s')
                     Right v' -> let
                                 RSIOE cmd2 = f v'
                                 in
                                 cmd2 e $! s'

instance
    MonadPlus (ReaderStateIOError env state)
    where
    mzero
        = fail "mzero"
    RSIOE cmd1 `mplus` RSIOE cmd2
        = RSIOE $
          \ e s -> do
                   res@(r', s') <- cmd1 e s
                   case r' of
                     Left  _  -> cmd2 e s'
                     Right _  -> return res
                   

instance
    MonadIO (ReaderStateIOError env state)
    where
    liftIO a
        = RSIOE $
          \ _e s -> do
                    r <- ( a >>= return . Right) `catchError` (\ e -> return (Left e))
                    case r of
                      Left msg -> return (Left $ "ioexc: " ++ show msg, s)
                      Right v' -> return (Right v',                     s)

instance
    MonadState state (ReaderStateIOError env state)
    where
    get
        = RSIOE $
          \ _e  s -> return (Right s, s)

    put s
        = RSIOE $
          \ _e _s -> return (Right (), s)

instance
    MonadReader env (ReaderStateIOError env state)
    where
    ask
        = RSIOE $
          \ e  s -> return (Right e, s)

    local f (RSIOE cmd)
        = RSIOE $
          \ e  s -> cmd (f e) s

instance
    MonadError String (ReaderStateIOError env state)
    where
    throwError er
        = RSIOE $
          \ _e s -> return (Left er, s)

    catchError (RSIOE f) handler
        = RSIOE $
          \ e s -> do
                   (r', s') <- f e s
                   case r' of
                     Left  e' -> let RSIOE h = handler e' in h e s'
                     Right v' -> return $ (Right v', s')

-- ------------------------------------------------------------

modifyIO                :: (state -> IO state) -> ReaderStateIOError env state ()
modifyIO f              = do
                          s0 <- get
                          s1 <- liftIO (f s0)
                          put s1

-- ------------------------------------------------------------

instance
    Functor (ReaderStateIOError env state)
    where
    fmap f xs = xs >>= return . f

instance
    Applicative (ReaderStateIOError env state)
    where
    pure = return
    (<*>) = ap

-- ------------------------------------------------------------
