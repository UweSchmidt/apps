module Main
where

import Control.Monad.RWSErrorIO

import System.Environment
import System.Exit
import System.IO

main :: IO ()
main
    = do args <- getArgs
         res <- evalAction (test args) Env ()
         maybe exitFailure return res

data Env = Env

instance Config Env where
    -- traceOn = const False    -- default is True
    -- stderrOn = const False   -- default is True

type Test = Action Env ()

test :: [String] -> Test ()
test args
    = do trc "script started"

         trc "io test"
         io $ putStrLn . show $ args

         trc "always test"
         always $ do io $ putStrLn "hello"
                     trc $ "hello"
                     _ <- abort "hello aborted"
                     io $ putStrLn "good bye"
                     trc $ "good bye"

         trc "orElse test"
         ( warn "abort will be called" >> abort "abort called" ) `orElse` trc "2. try"

         trc "catch io error test"
         ( io $ do h <- openFile "xxx" ReadMode
                   hClose h ) `orElse` (err "open file xxx has failed")

         trc "exec test"
         always $ exec "ls" ["-l", "."]

         trc "exec test which fails"
         always $ exec "xxx" ["-l", "."]

         trc "script finished"
         return ()

-- ----------------------------------------
