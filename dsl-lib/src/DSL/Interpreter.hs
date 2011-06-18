module DSL.Interpreter
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import Data.IORef

import System.IO

-- ------------------------------------------------------------

data Command instr
    = Noop
    | Quit
    | Incomplete String
    | SyntaxErr  String
    | Prog       instr

type Action env state res
    = ReaderStateIOError env state res

data Interpreter instr env state res
    = IP
      { _hello        ::           Action env state String
      , _bye          ::           Action env state String
      , _die          ::           Action env state String
      , _prompt       ::           Action env state String
      , _prompt2      ::           Action env state String
      , _readline     :: String -> Action env state (Maybe String)
      , _parse        :: String -> Action env state (Command instr)
      , _eval         ::  instr -> Action env state res
      , _print        ::    res -> Action env state ()
      , _interactive  ::           Action env state Bool
      }

type Exec instr env state res
    = Interpreter instr env state res -> Action env state ()

-- ------------------------------------------------------------

runInterpreter	:: Interpreter instr env state res -> env -> state -> IO (Either String (), state)
runInterpreter ip initEnv initState
    = runReaderStateIOError (runCommandLoop ip) initEnv initState

runCommandLoop :: Exec instr env state res
runCommandLoop i
    = do g <- _hello i
         message g
         commandLoop i

commandLoop :: Exec instr env state res
commandLoop i
    = do p <- _prompt i
         r <- _readline i p
         case r of
           Nothing -> commandLoopExit i		-- EOF on input
           Just l  -> commandParse l  i

commandParse :: String -> Exec instr env state res
commandParse l i
    = do cmd <- _parse i l
         commandExec cmd i

commandExec ::  Command instr -> Exec instr env state res
commandExec cmd i
    = case cmd of
        Noop
            -> commandLoop i
        Quit
            -> commandLoopExit i
        Incomplete part
            -> do p <- _prompt2  i
                  r <- _readline i p
                  case r of
                    Nothing
                        -> commandLoopErr ("EOF on input stream when parsing\n" ++ part) i
                    Just l
                        -> commandParse (part ++ "\n" ++ l) i
        SyntaxErr msg
            -> do ia <- _interactive i
                  if ia
                     then  message msg
                           >>
                           commandLoop i
                     else commandLoopErr msg i
        Prog instr
            -> commandEval instr i
               >>
               commandLoop i

commandEval :: instr -> Exec instr env state res
commandEval instr i
    = ( do r <- _eval i instr
           _print i r
      )
      `catchError` 
      ( \ e -> do ia <- _interactive i
                  if ia
                     then message e
                     else commandLoopErr e i
      )

commandLoopExit :: Exec instr env state res
commandLoopExit i
    = _bye i >>= message

commandLoopErr :: String -> Exec instr env state res
commandLoopErr msg i
    = do message msg
         die <- _die i
         message die
         throwError msg

-- ------------------------------------------------------------

message :: (MonadIO m) => String -> m ()
message msg
    = liftIO $
      when (not . null $ msg) $
           do hPutStrLn stderr msg
	      hFlush    stderr

prompt :: (MonadIO m) => String -> m ()
prompt pr
    = liftIO $
      when (not . null $ pr) $
           do hPutStr stdout pr
	      hFlush  stdout

readLine :: (MonadIO m) => String -> m (Maybe String)
readLine pr
    = do prompt pr
         liftIO $ do eof <- isEOF
	             if eof
	                then return Nothing
	                else getLine >>= return . Just

readWholeFile :: (MonadIO m) => FilePath -> IO (String -> m (Maybe String))
readWholeFile fn
    = do inputRead <- newIORef False
         return $ readF inputRead fn

readF :: MonadIO m => IORef Bool -> FilePath -> (String -> m (Maybe String))
readF ir fn
          = \ _prompt ->
            liftIO $
            do r <- readIORef ir
               if r
                  then return Nothing
                  else do writeIORef ir True
                          h <- openFile fn ReadMode
                          c <- hGetContents h
                          return $ Just c

-- ------------------------------------------------------------

scriptInterpreter	:: FilePath -> Interpreter instr env state res ->  IO (Interpreter instr env state res)
scriptInterpreter fn i
    = do rc <- readWholeFile fn
         return i { _interactive = return False
                  , _readline    = rc
                  , _prompt      = return ""
                  , _prompt2     = return ""
                  }

-- ------------------------------------------------------------
               
