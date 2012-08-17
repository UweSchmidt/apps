module Main
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import System.Environment
import System.Exit

import System.DirTree.Core
import System.DirTree.FindExpr
import System.DirTree.Types

import Text.Regex.XMLSchema.String	( matchRE )

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
      env = initFindPred . initCwd . setOpts $ initEnv


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
       <.> oUtf8
       <.> oScan				-- sequence of filter options is important
       <.> oTypes				-- expensive options first: oScan eval reads contents,
       <.> oMatchName <.> oNotMatchName 	-- oTypes eval reads file types, oMatch* does not need IO
       <.> oMatchPath <.> oNotMatchPath
       <.> oMatchExt  <.> oNotMatchExt
--     <.> oIsFile    <.> oNotIsFile
--     <.> oIsDir     <.> oNotIsDir
       <.> oFind
       <.> oGrep

-- ----------------------------------------
--
-- the option definitions

oVerbose :: Term (Env -> Env)
oVerbose
    = convFlag setVerbose
      $ (optInfo ["verbose"])
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
      $ (optInfo ["quiet"])
            { optDoc = "Turn off warnings and trace output." }
    where
      setQuiet True e
          = e { theTraceFlag   = False
              , theWarningFlag = False
              }
      setQuiet False e
          = e

oUtf8 :: Term (Env -> Env)
oUtf8
    = convFlag setUtf8
      $ (optInfo ["utf8"])
            { optDoc = "File contents are assumed to be utf8 encoded when processed by grep commands." }
    where
      setUtf8 True  e = e { theUtf8Flag = True }
      setUtf8 False e = e

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

oTypes :: Term (Env -> Env)
oTypes
    = convValue "illegal type given in type spec" setTypes
      $ (optInfo ["types", "T"])
            { optDoc = unwords [ "Test whether entry is one of the types specified in TYPES."
                               , "Every char in TYPES stands for a type,"
                               , "'d' stands for directory, 'f' for file."
                               , "Other types are not yet supported."
                               ]
            , optName = "TYPES"
            }
    where
      setTypes s
          | null s
              = Just id
          | all (`elem` "fd") s
              = Just $ addFindExpr typeExpr
          | otherwise
              = Nothing
          where
            c2e 'f' = IsFile
            c2e 'd' = IsDir
            c2e _   = FFalse
            typeExpr = orExpr . map c2e $ s

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

oFind :: Term (Env -> Env)
oFind
    = convFlag setFind
      $ (optInfo ["print", "p"])
            { optDoc = "Print matching file paths (default)." }
    where
      setFind True  e = e { theProcessor = genFindProcessor }
      setFind False e = e

oGrep :: Term (Env -> Env)
oGrep
    = convRegex setGrep
      $ (optInfo ["grep", "g"])
            { optName = "REGEXP"
            , optDoc = unwords [ "Find lines in all selected files matching REGEXP."
                               , "Context specs (^, $, \\<, \\>) like in egrep are allowed."
                               ]
            }
    where
      setGrep ""
          = return id
      setGrep s
          = fmap setG $ checkContextRegex s
          where
            setG re e
                = e { theProcessor = genGrepProcessor
                    , theGrepPred  = matchRE re
                    }

oScan :: Term (Env -> Env)
oScan
    = convValue "illegal scan function or regexp" setScan
      $ (optInfo ["scan", "s"])
            { optName = "SCAN-FCT-or-REGEXP"
            , optDoc = unwords [ "Test whether file contents has some feature given by SCAN-FCT."
                               , "SCAN-FCT may have the following values:"
                               , "'ascii', 'latin1', 'latin1+', 'unicode',"
                               , "'unicode+', 'utf8', 'utf8+', 'trailing-ws', 'contains-tabs'"
                               , "or it may be a regular expression like in --grep option."
                               , "Meaning:"
                               , "'latin1+': There are some none ascii chars,"
                               , "'utf8+': There are some none ascii chars (multi byte chars),"
                               , "'unicode+': There are some none latin1 chars."
                               , "REGEXP args are processed like in --grep functions but acts here as"
                               , "filter for selecting files."
                               ]
            }
    where
      setScan ""
          = return id
      setScan s
          = fmap addFindExpr $ checkScan s
          where
            checkScan "ascii"         = fe isAsciiText
            checkScan "latin1"        = fe isLatin1Text
            checkScan "latin1+"       = fe containsLatin1
            checkScan "unicode"       = fe isUnicodeText
            checkScan "unicode+"      = fe containsNoneLatin1
            checkScan "utf8"          = fe isUtf8
            checkScan "utf8+"         = fe isUtfText
            checkScan "trailing-ws"   = fe hasTrailingWSLine
            checkScan "contains-tabs" = fe containsTabs
            checkScan s'              = fmap grp $ checkContextRegex s'

            fe x                      = Just $ HasCont $ (return . x)

            grp re                    = HasCont $ (return . any (matchRE re) . lines)

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

