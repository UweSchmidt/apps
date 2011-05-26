module Main where

import Control.Exception

import Data.Maybe

import Photo2.CmdInterpreter
import Photo2.ArchiveTypes

import System.Environment
import System.Directory
import System.IO

import System.Console.Haskeline
import System.Console.Haskeline.IO

import Text.XML.HXT.Core ( stringTrim )

-- ------------------------------------------------------------

main    :: IO ()
main 
  = do
    getArgs >>= changeWD
    loop

changeWD :: [String] -> IO ()
changeWD []
  = return ()
changeWD (wd:_)
  = do 
    setCurrentDirectory wd
    nwd <- getCurrentDirectory
    hPutStrLn stderr $ "Working directory changed to " ++ show nwd

loop :: IO ()
loop 
  = do
    is0   <- initializeInput defaultSettings
    _     <- cmdLoop (readCmdLine is0) emptyAppState
    closeInput is0
    return ()
  
loop' :: IO ()
loop' 
  = do
    is0   <- initializeInput defaultSettings
    bracketOnError
      ( return is0 )
      ( cancelInput )
      ( \ is1 -> do
                 _ <- cmdLoop (readCmdLine is1) emptyAppState
                 closeInput is0
                 return ()
      )

readCmdLine     :: InputState -> String -> IO String
readCmdLine is0 prompt
  = do
    line <- queryInput is0 (getInputLine prompt)
    let line' = stringTrim . fromMaybe "" $ line
    if null line'
      then readCmdLine is0 prompt
      else return line'

-- ------------------------------------------------------------
