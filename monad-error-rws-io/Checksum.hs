module Main
where

import Control.Applicative 	()
import Control.Monad.RWSErrorIO

import System.Environment
import System.Exit

import System.DirTree.Core
import System.DirTree.Types

-- ----------------------------------------
--
-- module Main where
--
-- ----------------------------------------

main :: IO ()
main
    = do args <- getArgs
         prog <- getProgName
         env <- evalOptions prog args initEnv
         res <- evalAction doIt env initState
         maybe exitFailure return res

-- ----------------------------------------

evalOptions :: String -> [String] -> Env -> IO Env
evalOptions pn args env0
    = return $ initFindPred . initGrepPred . initCwd
             $ setOptions args
             $ env0 { theProgName = pn }
      where
        setOptions = const id

-- ----------------------------------------
