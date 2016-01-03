module Control.Monad.RWSErrorIO
    ( module Control.Monad.RWSErrorIO
    , module Control.Monad.Trans.Except
    , module Control.Monad.RWS
    , module Control.Monad
    )
where

import Control.Exception        ( SomeException
                                , try
                                )

import Control.Monad.Trans.Except
import Control.Monad.RWS
import Control.Monad

-- import Data.Monoid
import System.Process           ( rawSystem )
import System.Exit
import System.IO                ( hPutStrLn
                                , stderr
                                )

-- ----------------------------------------

type Action r s = ExceptT Msg (RWST r Log s IO)

runAction :: Action r s a -> r -> s -> IO (Either Msg a, s, Log)
runAction action env0 state0
    = runRWST (runExceptT action) env0 state0

evalAction :: Action r s a -> r -> s -> IO (Maybe a)
evalAction action env0 state0
    = do (res, _state1, lg) <- runAction action env0 state0

         -- issue log on stderr
         sequence_ . map (hPutStrLn stderr) . logToList $ lg

         -- issue error or return value
         either (\ x -> hPutStrLn stderr (unMsg x) >> return Nothing)
                (return . Just)
                res

-- ----------------------------------------

class Config c where
    traceOn :: c -> Bool
    traceOn = const True

    warningOn :: c -> Bool
    warningOn = const True

    errorOn :: c -> Bool
    errorOn = const True

    stderrOn :: c -> Bool
    stderrOn = const True

-- ----------------------------------------

newtype Msg = Msg String
    deriving (Show)

unMsg :: Msg -> String
unMsg (Msg s) = s

abort :: Config r => String -> Action r s a
abort msg
    = do err msg
         throwE . Msg $ msg

-- ----------------------------------------

data Log = LogEmpty
         | LogMsg String
         | Log2 Log Log
           deriving (Show)

instance Monoid Log where
    mempty = LogEmpty
    mappend LogEmpty l2 = l2
    mappend l1 LogEmpty = l1
    mappend l1 l2       = Log2 l1 l2

-- logging can be configured for stderr or for a writer protocol
-- the log level can be configured in the env

logg :: Config r => (r -> Bool) -> String -> String -> Action r s ()
logg enabled level msg
    = do asks enabled `guards`
           do s <- asks stderrOn
              if s
                 then io $ hPutStrLn stderr fmt
                 else tell . LogMsg $ fmt
    where
      fmt = take 10 (level ++ ":" ++ replicate 10 ' ') ++ msg

-- convenience functions for logging

trc :: Config r => String -> Action r s ()
trc = logg traceOn "message"

warn :: Config r => String -> Action r s ()
warn = logg warningOn "warning"

err :: Config r => String -> Action r s ()
err = logg errorOn "error"


logToList :: Log -> [String]
logToList = log' []
    where
      log' r (Log2 l1 l2) = log' (log' r l2) l1
      log' r (LogMsg s)   = s : r
      log' r  LogEmpty    = r

-- ----------------------------------------

io :: Config r => IO a -> Action r s a
io x
    = do r <- liftIO $ try x
         either (abort . showExc) return $ r
    where
      showExc :: SomeException -> String
      showExc = show

always :: Config r => Action r s () -> Action r s ()
always x
    = x `orElse` return ()

orElse :: Config r => Action r s a -> Action r s a -> Action r s a
orElse x1 x2
    = x1 `catchE` try2
    where
      try2 (Msg _s)
          = do -- warn $ "error ignored: " ++ s
               x2

finally :: Action r s a -> Action r s () -> Action r s a
finally x fin
    = ( do res <- x
           fin
           return res
      ) `catchE` (\ e -> fin >> throwE e)

guards :: Monad m => m Bool -> m () -> m ()
guards g x
    = do b <- g
         when b x

-- ----------------------------------------

-- execute external program

exec :: Config r => String -> [String] -> Action r s ()
exec cmd args
    = do rc <- io $ rawSystem cmd args
         if rc == ExitSuccess
            then return ()
            else abort $ unwords ["error in executing external program: ", cmd, show args]

-- convenience function for simple commands

execStr :: Config r => String -> Action r s ()
execStr cmd0
    = exec (concat . take 1 $ cmd) (drop 1 cmd)
    where
      cmd = words cmd0

-- ----------------------------------------
