module DSL.CommandLoop
where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import DSL.Base

import System.IO
import System.Exit

-- ------------------------------------------------------------

runApp :: ( InitialValue state
          , InitialValue env
          , Show err
          ) => ReaderStateIOError env state err res -> IO ()
runApp cmdLoop
    = do (res, _state) <- runReaderStateIOError cmdLoop initValue initValue
         either ( \ e -> hPutStrLn stderr (show $ e) >> exitFailure )
                (const exitSuccess)
                res
             
-- ------------------------------------------------------------

commandLoop	:: ( ReadLine   env
                   , IsInstr    instr
                   , ToError err
                   ) =>
                   (          ReaderStateIOError env state err String) ->
                   (          ReaderStateIOError env state err String) ->
                   (String -> ReaderStateIOError env state err instr)  ->
                   (instr  -> ReaderStateIOError env state err res)    ->
                   ReaderStateIOError env state err res

commandLoop greeting prompt parseL evalI
    = do greeting >>= message
         loop
    where
       loop
          = do p <- prompt
               e <- ask
               l <- liftIO $ getReadLine e p
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
           = do e <- ask
                l <- liftIO $ getReadLine e " > "
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

