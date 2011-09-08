{-# OPTIONS #-} {- XTypeSynonymInstances  -XFlexibleContexts -XFlexibleInstances -XMultiParamTypeClasses -}

module Language.Common.REPL
where

import Control.Applicative         ( (<$>) )

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Data.List                   ( isPrefixOf )

import Text.Regex.XMLSchema.String ( tokenize' )

import System.Directory            ( getCurrentDirectory )
import System.Exit
import System.FilePath             ( takeFileName )
import System.IO
import System.Posix.Env            ( getEnvDefault )

-- ------------------------------------------------------------

data Command prg
    = Noop
    | Quit
    | Incomplete String
    | SyntaxErr  String
    | Prog       prg

data REPLState prg state val err
    = REPLState
      { _hello        ::                 REPL prg state val err String
      , _bye          ::                 REPL prg state val err String
      , _prompt       ::                 REPL prg state val err String
      , _prompt2      ::                 REPL prg state val err String
      , _readline     ::       String -> REPL prg state val err (Maybe String)
      , _parse        ::       String -> REPL prg state val err (Command prg)
      , _eval         :: prg -> state -> REPL prg state val err (Either err val)
      , _printVal     ::          val -> REPL prg state val err ()
      , _printErr     ::          err -> REPL prg state val err ()
      , _finished     ::          Int -> REPL prg state val err ()
      , _cmdcnt       ::                 Int
      , _appstate     ::                 state
      }

type REPL prg state val err
    = StateT (REPLState prg state val err) IO

runREPL :: REPL prg state val err res -> REPLState prg state val err -> IO ()
runREPL action s0
    = do (_res, _s1) <- runStateT action s0
         return ()

repLoop0 :: REPL prg state val err ()
repLoop0
    = do s <- get
         g <- _hello s
         message g
         repLoop

repLoop :: REPL prg state val err ()
repLoop
    = do s <- get
         p <- _prompt s >>= promptSubst
         r <- _readline s p
         case r of
           Nothing -> repLoopExit 0		-- EOF on input
           Just l  -> repLoopParse l

repLoopExit :: Int -> REPL prg state val err ()
repLoopExit rc
    = do s <- get
         _bye s >>= message
         _finished s rc

repLoopParse :: String -> REPL prg state val err ()
repLoopParse l
    = do s <- get
         c <- _parse s l
         repLoopExec c

repLoopExec :: Command prg -> REPL prg state val err ()
repLoopExec (Noop)
    = repLoop

repLoopExec (Quit)
    = repLoopExit 0

repLoopExec (Incomplete part)
    = do s <- get
         p <- _prompt2 s
         r <- _readline s p
         case r of
           Nothing
               -> message ("EOF on input stream when parsing\n" ++ part) >>
                  repLoopExit 2
           Just l
               -> repLoopParse (part ++ "\n" ++ l)

repLoopExec (SyntaxErr msg)
    = do message msg
         repLoopCont
 
repLoopExec (Prog p)
    = do s <- get
         v <- (_eval s) p (_appstate s)
         repLoopPrint v

repLoopCont :: REPL prg state val err ()
repLoopCont
    = do modify $ \ s -> s { _cmdcnt = 1 + _cmdcnt s }
         repLoop

repLoopPrint :: (Either err val) -> REPL prg state val err ()
repLoopPrint v
    = ( case v of
          Left  err -> gets _printErr >>= ($ err)
          Right res -> gets _printVal >>= ($ res)
      ) >> repLoopCont

repLoopErr :: err -> REPL prg state val err ()
repLoopErr msg
    = gets _printErr >>= ($ msg)

repLoopTerm :: Int -> REPL prg state val err ()
repLoopTerm rc
    = do s <- get
         _finished s rc

promptSubst :: String -> REPL prg state val err String
promptSubst ps
    = concat <$> mapM substBS (tokenize' "\\\\." ps)
      where
        substBS (Left s)
            = return s
        substBS (Right "\\#")
            = do s <- get
                 return . show . _cmdcnt $ s
        substBS (Right "\\w")
            = cwd
        substBS (Right "\\W")
            = takeFileName <$> cwd
        substBS (Right "\\\\")
            = return "\\"
        substBS (Right s)
            = return (drop 1 s)
        cwd
            = liftIO $
              do d <- getCurrentDirectory
                 h <- getEnvDefault "HOME" "~"
                 return $ if h `isPrefixOf` d
                          then "~" ++ drop (length h) d
                          else d

-- ------------------------------------------------------------

finished :: (MonadIO m) => Int -> m ()
finished rc
    = liftIO $
      exitWith $
      if rc == 0
         then ExitSuccess
         else ExitFailure rc

printRes :: (MonadIO m) => String -> m ()
printRes ""
    = return ()
printRes res
    = liftIO $
      do hPutStrLn stdout res
	 hFlush    stdout

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
	                else getLine >>= return . isQuit
    where
      isQuit l
          | "\EOT" `isPrefixOf` l
              = Nothing
          | otherwise
              = Just l

-- ------------------------------------------------------------
