module Main where

import Control.Monad.ReaderStateIOError

import DSL.Base
import DSL.CommandLoop
import DSL.ReadLineLoop

-- ------------------------------------------------------------
--
-- this one works with standard getLine for input

main :: IO ()
main
    = runApp $
      commandLoop greetings prompt1 parseLine evalInstr

-- works with Haskeline line input, history and line editing

main1 :: IO ()
main1
    = runApp $
      withHaskeline $
      commandLoop greetings prompt1 parseLine evalInstr

-- ------------------------------------------------------------
--
-- syntactic domains

data Instr = Quit
           | Usage
           | Incomplete  String
           | Illegal     String
           | Ins        [String]
             deriving (Show)

instance IsQuitInstr        Instr where
    isQuit Quit             = Just "bye"
    isQuit _                = Nothing

instance IsIllegalInstr     Instr where
    isIllegal (Illegal msg) = Just msg
    isIllegal _             = Nothing

instance IsUsageInstr       Instr where
    isUsage Usage           = Just "the usage text"
    isUsage _               = Nothing

instance IsIncompleteInstr  Instr where
    isIncomplete (Incomplete part)
                            = Just part
    isIncomplete _          = Nothing

instance IsInstr            Instr where

-- ------------------------------------------------------------
--
-- semantic domains

type Cmd res   = ReaderStateIOError Env State Err res

-- ------------------------------------------------------------

newtype Env    = Env { _readLine :: String -> IO String }

instance InitialValue  Env where
    initValue        = Env { _readLine = readLineSimple }

instance ReadLine      Env where
    getReadLine      = _readLine
    setReadLine c e  = e { _readLine = c }

-- ------------------------------------------------------------

newtype State  = State { cmdCnt :: Int }

instance InitialValue State where
    initValue = State { cmdCnt = 0 }

-- ------------------------------------------------------------

data Err          = Err String

instance Show          Err where show (Err err)  = err
instance StringToError Err where stringToError   = Err
instance IOExcToError  Err where ioExcToError    = Err . show
instance ToError       Err where

-- ------------------------------------------------------------

greetings :: (Monad m) => m String
greetings
    = return "Command Loop Demo"

prompt1 :: Cmd String
prompt1 = do s <- gets cmdCnt
             return $ "cmd(" ++ show s ++ ") > "

parseLine :: (Monad m) => String -> m Instr
parseLine l
    = return . parse . words $ l
    where
      parse ["quit"]      = Quit
      parse ("quit" : _)  = Illegal "quit has no parameters"
      parse ws
          | length ws < 2 = Incomplete l
          | length ws > 5 = Illegal $ "too many arguments: " ++ unwords ws
          | otherwise     = Ins ws


evalInstr :: Instr -> Cmd ()
evalInstr (Ins ws)
    = do modify incr
         liftIO $ putStrLn . unwords $ ws
    where
      incr (State i) = State $ i + 1

evalInstr _
    = return ()

-- ------------------------------------------------------------
