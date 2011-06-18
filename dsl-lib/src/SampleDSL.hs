module SampleDSL
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.ReaderStateIOError

import System.IO
import System.Exit

import DSL.Interpreter

-- ------------------------------------------------------------

type Instr = [String]

data Env = Env

data State = State Int

type Res = Instr

-- ------------------------------------------------------------

type Cmd res = Action Env State res

-- ------------------------------------------------------------

interpreter :: Interpreter Instr Env State Res
interpreter
    = IP
      { _hello    = return version
      , _bye      = return "normal exit of sample command line interpreter"
      , _die      = return "sample command line interpreter aborted by exception"
      , _prompt   = ( do
		      (State cnt) <- get
		      return $ "cmd(" ++ show cnt ++ ")> "
		    )
      , _prompt2  = return "more input please > "
      , _readline = readLine
      , _parse    = parseInp
      , _eval     = evalInstr
      , _print    = printRes
      , _interactive = return True
      }
    where
      printRes res
          = liftIO $ do hPutStrLn stdout $ unwords res
                        hFlush    stdout
      parseInp inp
          = return . parseWords . words $ inp
          where
            parseWords [] = Noop
            parseWords ["quit"] = Quit
            parseWords ("quit" : _ ) = SyntaxErr "quit does not have any arguments"
            parseWords ws
                | head ws `elem` ["?", "help"] = Prog ws
                | length ws < 2 = Incomplete inp
                | otherwise     = Prog ws

version :: String
version = "Sample command line interpreter, version 0.0.0"

usage :: String
usage = unlines $
        [ version
        , ""
        , "commands:"
        , "  cmd arg...  execute command"
        , "  quit        exit application"
        , "  ?, help     this text"
        ]
              
evalInstr :: Instr -> Cmd Res
evalInstr ["?"]
    = do message usage
         return []
evalInstr ("xxx":_)
    = throwError "eval: no xxx commands please"
evalInstr ins
    | head ins `elem` ["?", "help"]
          = message usage
            >> return []
    | otherwise
        = do modify (\ (State i) -> State $ i + 1)
             return ins
            
-- ------------------------------------------------------------

main :: IO (Either String (), State)
main
    = do (res, State i) <- runInterpreter interpreter Env (State 0)
         case res of
           Left _
               -> do -- hPutStrLn stderr msg
                     exitFailure
           Right _
               -> do hPutStrLn stderr $ show i ++ " command(s) executed"
                     exitSuccess

-- ------------------------------------------------------------
