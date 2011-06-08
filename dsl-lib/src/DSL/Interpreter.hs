module DSL.Interpreter
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import DSL.Base

import System.IO
import System.Exit

-- ------------------------------------------------------------

data Interpreter instr env state err
    = IP
      { _greetings    ::                 Command instr env state err String
      , _prompt       ::                 Command instr env state err String
      , _readline     ::       String -> Command instr env state err (Maybe String)
      , _parse        :: Maybe String -> Command instr env state err instr
      , _eval         ::        instr -> Command instr env state err ()
      , _isIncomplete ::        instr -> Command instr env state err String
      , _isIllegal    ::        instr -> Command instr env state err String
      , _isQuit       ::        instr -> Command instr env state err String
      }

type Command instr env state err res = ReaderStateIOError (Interpreter instr env state err, env) state err res

-- ------------------------------------------------------------

runInterpreter	:: (StringToError err, IOExcToError err) =>
		   Interpreter instr env state err -> env -> state -> IO (Either err res, state)
runInterpreter ip initEnv initState
    = runReaderStateIOError startCommandLoop (ip, initEnv) initState

startCommandLoop :: (StringToError err, IOExcToError err) =>
		    Command instr env state err res
startCommandLoop
    = do
      e <- ask
      g <- _greetings (fst e)
      message g
      commandLoop

commandLoop = undefined

-- ------------------------------------------------------------

message :: (MonadIO m) => String -> m ()
message msg
    = liftIO $
      when (not . null $ msg) $
           do hPutStrLn stderr msg
	      hFlush stderr

readLine :: (MonadIO m) => String -> m (Maybe String)
readLine prompt
    = liftIO $
      do hPutStr stdout prompt
	 hFlush  stdout
	 eof <- isEOF
	 if eof
	    then return Nothing
	    else getLine >>= return . Just

-- ------------------------------------------------------------
-- example code
-- ------------------------------------------------------------

data Instr
    = NOOP
    | Usage String
    | Illegal String
    | Incomplete String
    | Exec [String]

data Env = Env

data State = State Int

data Err = Err Int String
	   deriving (Show)

instance StringToError Err where stringToError s = Err 2 s
instance IOExcToError  Err where ioExcToError e  = Err 2 (show e)

errL   = 2
warnL  = 1
fatalL = 3
okL    = 0

-- ------------------------------------------------------------

type Cmd res = Command Instr Env State Err res

-- ------------------------------------------------------------

interpreter :: Interpreter Instr Env State Err
interpreter
    = IP
      { _greetings = return "Sample command line interpreter 0.0.0"
      , _prompt = ( do
		    (State cnt) <- get
		    return $ "cmd(" ++ show cnt ++ ")> "
		  )
      , _readline = readLine

      , _parse = undefined
      , _eval = undefined
      , _isIncomplete = undefined
      , _isIllegal = undefined
      , _isQuit = undefined
      }

-- ------------------------------------------------------------


