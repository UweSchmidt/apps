module DSL.Interpreter
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import DSL.Base

import System.IO
import System.Exit

-- ------------------------------------------------------------

data Command instr
    = Noop
    | Quit
    | Incomplete String
    | SyntaxErr  String
    | Prog       instr

data Interpreter instr env state
    = IP
      { _greetings    ::           Exec instr env state String
      , _prompt       ::           Exec instr env state String
      , _prompt2      ::           Exec instr env state String
      , _readline     :: String -> Exec instr env state (Maybe String)
      , _parse        :: String -> Exec instr env state instr
      , _eval         ::  instr -> Exec instr env state ()
      , _isIncomplete ::  instr -> Maybe String
      , _isIllegal    ::  instr -> Exec instr env state String
      , _isQuit       ::  instr -> Exec instr env state String
      }

type Exec instr env state res = ReaderStateIOError (Interpreter instr env state, env) state res

-- ------------------------------------------------------------

runInterpreter	:: Interpreter instr env state -> env -> state -> IO (Either String (), state)
runInterpreter ip initEnv initState
    = runReaderStateIOError startCommandLoop (ip, initEnv) initState

startCommandLoop :: Exec instr env state ()
startCommandLoop
    = do i <- getInterpreter
         g <- _greetings i
         message g
         commandLoop

commandLoop :: Exec instr env state ()
commandLoop
    = do i <- getInterpreter
         p <- _prompt i
         r <- _readline i p
         case r of
           Nothing -> commandLoopExit		-- EOF on input
           Just l  -> commandParse l

commandParse :: String ->
                Exec instr env state ()
commandParse l
    = do i   <- getInterpreter
         ins <- _parse i l
         commandEval ins

commandEval ::  instr ->
                Exec instr env state ()
commandEval ins
    = do i   <- getInterpreter
         case _isIncomplete i ins of
           Nothing   -> commandEval1 i ins
           Just part -> readMore     i part
    where
      readMore i part
          = do p <- _prompt2 i
               r <- _readline i p
               case r of
                 Nothing -> fail "EOF on input file"
                 Just l  -> commandParse (part ++ "\n" ++ l)
      commandEval1 i ins
          = undefined

commandLoopExit :: Exec instr env state ()
commandLoopExit
    = undefined


getInterpreter :: Exec instr env state (Interpreter instr env state)
getInterpreter = fst <$> ask

getEnv :: Exec instr env state env
getEnv = snd <$> ask

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
    | Exec [String]

data Env = Env

data State = State Int

-- ------------------------------------------------------------

type Cmd res = Exec Instr Env State res

-- ------------------------------------------------------------

interpreter :: Interpreter Instr Env State
interpreter
    = IP
      { _greetings = return "Sample command line interpreter 0.0.0"
      , _prompt = ( do
		    (State cnt) <- get
		    return $ "cmd(" ++ show cnt ++ ")> "
		  )
      , _prompt2 = undefined
      , _readline = readLine

      , _parse = undefined
      , _eval = undefined
      , _isIncomplete = undefined
      , _isIllegal = undefined
      , _isQuit = undefined
      }

-- ------------------------------------------------------------


