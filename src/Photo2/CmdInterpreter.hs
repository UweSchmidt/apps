module Photo2.CmdInterpreter
where

import Photo2.ArchiveTypes

import qualified Control.Monad as CM

import Data.Maybe

import System.IO
import System.Console.Readline
    ( readline
    , addHistory
    )

import Text.XML.HXT.Arrow

type Cmd = AppState -> IO AppState

cmdLoop	:: Cmd
cmdLoop state
    = do
      cmd  <- getCmd
      if isNothing cmd
	 then return state
	 else do
	      newState <- (fromJust cmd) state
	      cmdLoop newState

getCmd	:: IO (Maybe Cmd)
getCmd
    = do
      line <- readCmdLine
      cmd  <- uncurry parseCmd (scanLine line)
      return cmd


prompt	:: String
prompt	= "photo2 > "

readCmdLine	:: IO String
readCmdLine
    = do
      line <- readline prompt
      let line' = stringTrim . fromMaybe "" $ line
      CM.when (length line' > 1) (addHistory line')
      if null line'
	 then readCmdLine
	 else return line'

parseCmd	:: String -> [String] -> IO (Maybe Cmd)
parseCmd "exit" _
    = return Nothing

parseCmd c args
    = return . Just
      $ \ s -> ( do
	         putStrLn ("unknown command: " ++ unwords (c : args))
	         return s )

scanLine	:: String -> (String, [String])
scanLine s
    | null args = ("",[])
    | otherwise	= (head args, tail args)
    where
    args = words s
