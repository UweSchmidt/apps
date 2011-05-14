module Photo2.ModelInterface
where

import           Data.IORef

import Photo2.ArchiveTypes
import Photo2.CmdInterpreter

-- ------------------------------------------------------------

type Model      = AppState

buildModel      :: IO Model
buildModel      = return emptyAppState

execModel       :: (String -> IO ()) ->
                   String ->
                   Model -> IO (String, Model)
execModel logger cmd s0
    = do
      resVar <- newIORef []
      s1  <- return $
             s0 { writeLog = logger
                , writeRes = addTo resVar
                }
      s2  <- runCmds cmds s1
      res <- readIORef resVar
      return (unlines . reverse $ res, s2)
    where
    addTo :: IORef [String] -> String -> IO ()
    addTo var val
        = do
          modifyIORef var (val :)

    cmds = parseCmdLine . tokenizeCmdLine $ cmd

    runCmds [] s0'
        = return s0'
    runCmds (c:cs) s0'
        = do
          s1' <- c s0'
          runCmds cs s1'

-- ------------------------------------------------------------
