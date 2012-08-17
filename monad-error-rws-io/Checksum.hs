module Main
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import System.Environment
import System.Exit

import System.DirTree.Core
import System.DirTree.FindExpr
import System.DirTree.Types

import System.Console.CmdTheLine

import Text.PrettyPrint

-- ----------------------------------------
--
-- module Main where
--
-- ----------------------------------------

main1 :: IO ()
main1
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

main :: IO ()
main = run (findGrepSed <$> oAll, fgsInfo)

findGrepSed :: (Env -> Env) -> IO ()
findGrepSed setOpts
    = do res <- evalAction doIt env initState
         maybe exitFailure return res
    where
      env = initFindPred . initGrepPred . initCwd . setOpts $ initEnv


fgsInfo :: TermInfo
fgsInfo
    = defTI
      { termName = "find-grep-sed"
      , version  = "0.0.0"
      }

oAll :: Term (Env -> Env)
oAll = oVerbose
       <.> oQuiet
       <.> oBackup
       <.> oMatchName <.> oNotMatchName
       <.> oMatchPath <.> oNotMatchPath
       <.> oMatchExt  <.> oNotMatchExt
       <.> oIsFile    <.> oNotIsFile
       <.> oIsDir     <.> oNotIsDir

-- ----------------------------------------
--
-- the option definitions

oVerbose :: Term (Env -> Env)
oVerbose
    = convFlag setVerbose
      $ (optInfo ["verbose", "v"])
            { optDoc = "Turn on trace output." }
    where
      setVerbose True e
          = e { theTraceFlag   = True
              , theWarningFlag = True
              }
      setVerbose False e
          = e

oQuiet :: Term (Env -> Env)
oQuiet
    = convFlag setQuiet
      $ (optInfo ["quiet", "q"])
            { optDoc = "Turn off warnings and trace output." }
    where
      setQuiet True e
          = e { theTraceFlag   = False
              , theWarningFlag = False
              }
      setQuiet False e
          = e

oBackup :: Term (Env -> Env)
oBackup
    = fmap setBackup . value . opt "~"
      $ (optInfo ["backup", "b"])
            { optName = "BACKUP-SUFFIX"
            , optDoc  = unwords
                        [ "Make backup file when changing contents"
                        , "in sed commands."
                        , "Default is yes with suffix \"~\" (tilde)."
                        , "Empty suffix disables backup files."
                        ]
            }
    where
      setBackup "" e
          = e { theCreateBackup = False }
      setBackup s  e
          = e { theCreateBackup = True
              , theBackupName   = (++ s)
              }

oIsFile :: Term (Env -> Env)
oIsFile
    = convFlag setIsFile
      $ (optInfo ["file", "f"])
            { optDoc = "Test whether entry is a file." }
    where
      setIsFile True  = addFindExpr IsFile
      setIsFile False = id

oNotIsFile :: Term (Env -> Env)
oNotIsFile
    = convFlag setNotIsFile
      $ (optInfo ["not-file", "F"])
            { optDoc = "Test whether entry isn't a file." }
    where
      setNotIsFile True  = addFindExpr $ NotExpr IsFile
      setNotIsFile False = id

oIsDir :: Term (Env -> Env)
oIsDir
    = convFlag setIsDir
      $ (optInfo ["dir", "d"])
            { optDoc = "Test whether entry is a directory." }
    where
      setIsDir True  = addFindExpr IsDir
      setIsDir False = id

oNotIsDir :: Term (Env -> Env)
oNotIsDir
    = convFlag setNotIsDir
      $ (optInfo ["not-dir", "D"])
            { optDoc = "Test whether entry isn't a directory." }
    where
      setNotIsDir True  = addFindExpr $ NotExpr IsDir
      setNotIsDir False = id

oMatchName :: Term (Env -> Env)
oMatchName
    = convRegex setMatchName
      $ (optInfo ["name", "n"])
            { optName = "REGEXP"
            , optDoc = "Test whether file name matches REGEXP"
            }
    where
      setMatchName = setFindRegex MatchRE

oNotMatchName :: Term (Env -> Env)
oNotMatchName
    = convRegex setMatchName
      $ (optInfo ["not-name", "N"])
            { optName = "REGEXP"
            , optDoc = "Test whether file name does not match REGEXP"
            }
    where
      setMatchName = setFindRegex (NotExpr . MatchRE)

oMatchPath :: Term (Env -> Env)
oMatchPath
    = convRegex setMatchPath
      $ (optInfo ["path", "p"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path matches REGEXP"
            }
    where
      setMatchPath = setFindRegex MatchPathRE

oNotMatchPath :: Term (Env -> Env)
oNotMatchPath
    = convRegex setMatchPath
      $ (optInfo ["not-path", "P"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path does not match REGEXP"
            }
    where
      setMatchPath = setFindRegex (NotExpr . MatchPathRE)

oMatchExt :: Term (Env -> Env)
oMatchExt
    = convRegex setMatchExt
      $ (optInfo ["extension", "e"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path matches REGEXP"
            }
    where
      setMatchExt = setFindRegex MatchExtRE

oNotMatchExt :: Term (Env -> Env)
oNotMatchExt
    = convRegex setMatchExt
      $ (optInfo ["not-extension", "E"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path does not match REGEXP"
            }
    where
      setMatchExt = setFindRegex (NotExpr . MatchExtRE)

-- ----------------------------------------
--
-- mothers little helpers

convRegex :: (String -> Maybe b) -> OptInfo -> Term b
convRegex
    = convValue "illegal regular expression"

setFindRegex :: (Regex -> FindExpr) -> String -> Maybe (Env -> Env)
setFindRegex _ ""
    = return id
setFindRegex constr s
    = fmap setFind $ checkRegex s
    where
      setFind re e
          =  e { theUserFindExpr = andExpr2 (theUserFindExpr e) (constr re) }

addFindExpr :: FindExpr -> (Env -> Env)
addFindExpr fe e
    =  e { theUserFindExpr = andExpr2 (theUserFindExpr e) fe }

-- ----------------------------------------
--
-- CmdTheLine utils
--
-- ----------------------------------------

-- an alias for (.) from Control.Category to compose terms of functions

(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) = liftA2 (.)

convertIO' :: (a -> String) -> (a -> IO (Maybe b)) -> String -> Term a -> Term b
convertIO' showArg conv msg
    = ret . fmap check
    where
      check v
          = do res <- liftIO $ conv v
               case res of
                 Nothing -> msgFail $ msg'
                 Just r  -> return r
          where
            msg' = sep [text msg, quotes . text . showArg $ v]

convertIO :: Show a => (a -> IO (Maybe b)) -> String -> Term a -> Term b
convertIO = convertIO' show

convert' :: (a -> String) -> (a -> Maybe b) -> String -> Term a -> Term b
convert' sf cv = convertIO' sf (return . cv)

convert :: Show a => (a -> Maybe b) -> String -> Term a -> Term b
convert = convert' show

convertString :: (String -> Maybe b) -> String -> Term String -> Term b
convertString = convert' id

convFlag :: (Bool -> b) -> OptInfo -> Term b
convFlag setFct
    = fmap setFct . value . flag

convValue :: String -> (String -> Maybe b) -> OptInfo -> Term b
convValue msg setFct
    = convertString setFct msg . value . opt ""

-- ----------------------------------------

