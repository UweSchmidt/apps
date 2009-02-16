module Photo2.ModelInterface
where

import Photo2.ArchiveTypes
import Photo2.CmdInterpreter

-- ------------------------------------------------------------

type Model	= AppState

initModel	= emptyAppState

execModel	:: (String -> IO ()) ->
		   (String -> IO ()) ->
		   String ->
		   AppState -> IO AppState
execModel evalRes logger cmd s0
    = runCmds cmds s1
    where
    s1 = s0 { writeLog = logger
	    , writeRes = evalRes
	    }

    cmds = parseCmdLine . tokenizeCmdLine $ cmd

    runCmds [] s0'
        = return s0'
    runCmds (c:cs) s0'
        = do
          s1' <- c s0'
          runCmds cs s1'

-- ------------------------------------------------------------
