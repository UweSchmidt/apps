module DSL.CommandLoop
where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import System.IO
import System.Exit

-- ------------------------------------------------------------

class IsQuitInstr a where
    isQuit :: a -> Maybe String
    isQuit _ = Nothing

class IsIllegalInstr a where
    isIllegal :: a -> Maybe String
    isIllegal _ = Nothing

class IsUsageInstr a where
    isUsage :: a -> Maybe String
    isUsage _ = Nothing

class IsIncompleteInstr a where
    isIncomplete :: a -> Maybe String
    isIncomplete _ = Nothing

-- ------------------------------------------------------------

class InitialValue a where
    initValue :: a

-- ------------------------------------------------------------

runLoop :: ( InitialValue env
           , InitialValue state
           , Show err
           ) => ReaderStateIOError env state err res -> IO ()
runLoop cmdLoop'
    = do (res, _state) <- runReaderStateIOError cmdLoop' initValue initValue
         either ( \ e -> hPutStrLn stderr (show $ e) >> exitFailure )
                (const exitSuccess)
                res
             
-- ------------------------------------------------------------

commandLoop	:: ( MonadIO           m
                   , IsQuitInstr       instr
                   , IsIncompleteInstr instr
                   , IsIllegalInstr    instr
                   , IsUsageInstr      instr
                   ) =>
                   (          m String) -> 
                   (          m String) ->
                   (String -> m String) ->
                   (String -> m instr)  ->
                   (instr  -> m res)    ->
                   m res
commandLoop greeting prompt readL parseL evalI
    = do greeting >>= message
         loop
    where
       loop
          = do p <- prompt
               l <- readL p
               i <- parseL l
               eval0 i
       eval0 i
           = maybe (eval1 i) readMore (isIncomplete i)
       eval1 i
           = maybe (eval2 i) messageAndLoop (isIllegal i)
       eval2 i
           = maybe (eval3 i) messageAndLoop (isUsage i)
       eval3 i
           = do r <- evalI i
                maybe loop ( \ msg ->
                             do message msg
                                return r
                           ) (isQuit i)
       readMore part
           = do
             l <- readL " > "
             i <- parseL (part ++ "\n" ++ l)
             eval0 i

       messageAndLoop msg
           = message msg >> loop

       message msg
           = liftIO $
             when (not . null $ msg) $
                  do hPutStrLn stderr msg
                     hFlush stderr

-- ------------------------------------------------------------

