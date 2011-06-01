module DSL.ReadLineLoop
where

import Control.Exception
import Control.Monad.ReaderStateIOError

import DSL.Base

import System.Console.Haskeline
import System.Console.Haskeline.IO

-- ------------------------------------------------------------

withHaskeline :: ( ReadLine env
                 , ToError  err
                 ) =>
                 ReaderStateIOError env state err res ->
                 ReaderStateIOError env state err res
withHaskeline cmd
    = do e <- ask
         s <- get
         (res, s') <- liftIO $ runWithHaskeline e s
         put s'
         either throwError return res
    where
      runWithHaskeline e s
          = bracketOnError
            (initializeInput defaultSettings)
            cancelInput
            (\hd -> do r <- cmd' hd
                       closeInput hd
                       return r
            )
          where
            cmd' hd
                = runReaderStateIOError cmd (setReadLine readHaskeline e) s
                where
                  readHaskeline p
                      = do l <- queryInput hd (getInputLine p)
                           case l of
                             Nothing -> return ""
                             Just x  -> return x

-- ------------------------------------------------------------
