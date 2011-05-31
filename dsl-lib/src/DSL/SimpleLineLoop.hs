module DSL.SimpleLineLoop
where

import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import DSL.CommandLoop

import System.IO

-- ------------------------------------------------------------

readSimpline :: (MonadIO m) => String -> m String
readSimpline p
    = do l <- readLine'
         if null l
            then readSimpline p
            else return l
    where
      readLine'
          = liftIO $
            do hPutStr stdout p
               hFlush stdout
               getLine

-- ------------------------------------------------------------

readSimpleLoop :: ( IsQuitInstr       instr
                  , IsIllegalInstr    instr
                  , IsUsageInstr      instr
                  , IsIncompleteInstr instr
                  , InitialValue      state
                  , InitialValue      env
                  , IOExcToError      err
                  , ToError           err
                  , Show              err
                  ) =>
                            ReaderStateIOError env state err String ->
                            ReaderStateIOError env state err String ->
                 (String -> ReaderStateIOError env state err instr) ->
                 (instr  -> ReaderStateIOError env state err res  ) ->
                 IO ()
readSimpleLoop greetings prompt parseLine evalInstr
    = runLoop $ cmdLoop
    where
      cmdLoop = commandLoop greetings prompt readSimpline parseLine evalInstr

-- ------------------------------------------------------------
