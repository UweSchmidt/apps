module DSL.ReadLineLoop
where


import Control.Exception
import Control.Monad.ReaderStateIOError
import Control.Monad.Trans

import DSL.CommandLoop

import System.Console.Haskeline
import System.Console.Haskeline.IO

-- ------------------------------------------------------------

readLineLoop :: ( IsQuitInstr       instr
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
readLineLoop greetings prompt parseLine evalInstr
    = bracketOnError
      (initializeInput defaultSettings)
      cancelInput
      (\hd -> loop hd >> closeInput hd)
    where
      loop :: InputState -> IO ()
      loop hd
          = runLoop cmdLoop'
          where
            cmdLoop'
                = commandLoop
                  greetings
                  prompt
                  (liftIO . readHaskeline)
                  parseLine
                  evalInstr

            readHaskeline p
                = do l <- queryInput hd (getInputLine p)
                     case l of
                       Nothing -> return ""
                       Just s  -> return s

-- ------------------------------------------------------------
