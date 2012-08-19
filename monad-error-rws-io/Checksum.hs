module Main
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.Maybe

import System.Exit

import System.DirTree.Core
import System.DirTree.FindExpr
import System.DirTree.Types
import System.DirTree.CmdTheLine

-- ----------------------------------------
--
-- module Main where
--
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
       <.> oReadUtf8 <.> oWriteUtf8 <.> oUtf8
       <.> oMatchName <.> oNotMatchName
       <.> oMatchPath <.> oNotMatchPath
       <.> oMatchExt  <.> oNotMatchExt
       <.> oScan
       <.> oTypes
       <.> oFind
       <.> oGrep
       <.> oSed

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
            { optDoc = "Shortcut for --read-utf8 --write-utf8." }
    where
      setUtf8 True  e = e { theUtf8DecFlag = True
                          , theUtf8EncFlag = True
                          }
      setUtf8 False e = e

oReadUtf8 :: Term (Env -> Env)
oReadUtf8
    = convFlag setUtf8
      $ (optInfo ["read-utf8"])
            { optDoc = "File contents are assumed to be utf8 encoded when reading files in grep and sed commands." }
    where
      setUtf8 True  e = e { theUtf8DecFlag = True }
      setUtf8 False e = e

oWriteUtf8 :: Term (Env -> Env)
oWriteUtf8
    = convFlag setUtf8
      $ (optInfo ["write-utf8"])
            { optDoc = "File contents are utf8 encoded when writing files in sed commands." }
    where
      setUtf8 True  e = e { theUtf8EncFlag = True }
      setUtf8 False e = e

oBackup :: Term (Env -> Env)
oBackup
    = fmap setBackup . value . opt "~"
      $ (optInfo ["backup"])
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
    = convStringValue "illegal type given in type spec" setTypes
      $ (optInfo ["types"])
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
            typeExpr = orExprSeq . map c2e $ s

oMatchName :: Term (Env -> Env)
oMatchName
    = convRegexSeq setMatchName
      $ (optInfo ["name"])
            { optName = "REGEXP"
            , optDoc = "Test whether file name matches REGEXP."
            }
    where
      setMatchName = setFindRegex MatchRE

oNotMatchName :: Term (Env -> Env)
oNotMatchName
    = convRegexSeq setMatchName
      $ (optInfo ["not-name"])
            { optName = "REGEXP"
            , optDoc = "Test whether file name does not match REGEXP."
            }
    where
      setMatchName = setFindRegex (NotExpr . MatchRE)

oMatchPath :: Term (Env -> Env)
oMatchPath
    = convRegexSeq setMatchPath
      $ (optInfo ["path"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path matches REGEXP."
            }
    where
      setMatchPath = setFindRegex MatchPathRE

oNotMatchPath :: Term (Env -> Env)
oNotMatchPath
    = convRegexSeq setMatchPath
      $ (optInfo ["not-path"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path does not match REGEXP."
            }
    where
      setMatchPath = setFindRegex (NotExpr . MatchPathRE)

oMatchExt :: Term (Env -> Env)
oMatchExt
    = convRegexSeq setMatchExt
      $ (optInfo ["ext"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path matches REGEXP."
            }
    where
      setMatchExt = setFindRegex MatchExtRE

oNotMatchExt :: Term (Env -> Env)
oNotMatchExt
    = convRegexSeq setMatchExt
      $ (optInfo ["not-ext"])
            { optName = "REGEXP"
            , optDoc = "Test whether whole path does not match REGEXP."
            }
    where
      setMatchExt = setFindRegex (NotExpr . MatchExtRE)

oScan :: Term (Env -> Env)
oScan
    = convStringSeqValue "illegal scan function or regexp" setScan
      $ (optInfo ["scan"])
            { optName = "SCAN-FCT-or-REGEXP"
            , optDoc = unwords [ "Test whether file contents has some feature given by SCAN-FCT."
                               , "SCAN-FCT may have the following values:"
                               , "'/ascii/', '/latin1/', '/latin1-ascii/', '/unicode/',"
                               , "'/unicode-latin1/', '/utf8/', '/utf8-ascii/', '/trailing-ws/', '/tabs/'"
                               , "or it may be a regular expression like in --grep option."
                               , "\n"
                               , "Meaning:"
                               , "'/latin1-ascii/': Latin1 with some none ascii chars,"
                               , "'/utf8/': Utf8 with some utf8 multi byte chars,"
                               , "'/unicode-latin1/': Unicode with some none latin1 chars."
                               , "REGEXP args are processed like in --grep functions but acts here as"
                               , "filter for selecting files."
                               , "\n"
                               , "No Utf8 decoding is done in scan operations, input is read as bytestring."
                               ]
            }
    where
      setScan ""
          = return id
      setScan s
          = fmap addFindExpr $ checkScan s
          where
            checkScan "/ascii/"          = fe isAsciiText
            checkScan "/latin1/"         = fe isLatin1Text
            checkScan "/latin1-ascii/"   = fe containsLatin1
            checkScan "/unicode/"        = fe isUnicodeText
            checkScan "/unicode-latin1/" = fe containsNoneLatin1
            checkScan "/utf8/"           = fe isUtf8
            checkScan "/utf8-ascii/"     = fe isUtfText
            checkScan "/trailing-ws/"    = fe hasTrailingWSLine
            checkScan "/tabs/        "   = fe containsTabs
            checkScan s'                 = fmap grp $ checkContextRegex s'

            fe x                         = Just $ HasCont $ (return . x)

            grp re                       = HasCont $ (return . any (matchRE re) . lines)

-- ----------------------------------------
--
-- the action opions

oFind :: Term (Env -> Env)
oFind
    = convFlag setFind
      $ (optInfo ["print"])
            { optDoc = "Print matching file paths (default)." }
    where
      setFind True  e = e { theProcessor = genFindProcessor }
      setFind False e = e

oGrep :: Term (Env -> Env)
oGrep
    = convStringValue "illegal grep function or regexp" setGrep
      $ (optInfo ["grep"])
            { optName = "GREP-FCT-or-REGEXP"
            , optDoc = unwords [ "Find lines in all selected files matching REGEXP."
                               , "Context specs (^, $, \\<, \\>) like in egrep are allowed."
                               , "The arguments in --scan may also be used here as GREP-FCT,"
                               , "e.g. '--grep /tabs/' lists all lines containing tabs,"
                               ,"'--grep /latin1-ascii/' lists all lines containing none ascii latin1 chars."
                               ]
            }
    where
      setGrep ""
          = return id
      setGrep s
          = fmap setG $ checkGrep s
          where
            checkGrep "/ascii/"          = Just isAsciiText
            checkGrep "/latin1/"         = Just isLatin1Text
            checkGrep "/latin1-ascii/"   = Just containsLatin1
            checkGrep "/unicode/"        = Just isUnicodeText
            checkGrep "/unicode-latin1/" = Just containsNoneLatin1
            checkGrep "/utf8/"           = Just isUtf8
            checkGrep "/utf8-ascii/"     = Just isUtfText
            checkGrep "/trailing-ws/"    = Just hasTrailingWSLine
            checkGrep "/tabs/        "   = Just containsTabs
            checkGrep s'                 = fmap matchRE $ checkContextRegex s'

            setG p e
                = e { theProcessor = genGrepProcessor
                    , theGrepPred  = p
                    }

oSed :: Term (Env -> Env)
oSed
    = convStringValue "illegal sed function" setSed
      $ (optInfo ["sed"])
            { optName = "SED-FCT"
            , optDoc = unwords [ "Edit files with given edit function."
                               , "currently supported edit functions are:"
                               , "'/umlauts-to-ascii/', '/umlauts-to-tex/',"
                               , "'/html-to-ascii/', '/haskell-to-ascii/', '/tcl-to-ascii/',"
                               , "'/no-trailing-ws/', '/no-tabs/'."
                               ]
            }
    where
      setSed ""
          = return id
      setSed s
          = fmap setS $ checkSed s
          where
            checkSed "/umlauts-to-ascii/" = Just substUmlauts
            checkSed "/umlauts-to-tex/"   = Just substUmlautsTex
            checkSed "/html-to-ascii/"    = Just substXhtmlChars
            checkSed "/haskell-to-ascii/" = Just substToAsciiHaskell
            checkSed "/tcl-to-ascii/"     = Just substLatin1Tcl
            checkSed "/no-trailing-ws/"   = Just removeTrailingWS
            checkSed "/no-tabs/"          = Just removeTabs
            checkSed _                    = Nothing

            setS f e
                = e { theProcessor = genSedProcessor
                    , theSedFct  = f
                    }

-- ----------------------------------------
--
-- mothers little helpers

convRegex :: (String -> Maybe (b -> b)) -> OptInfo -> Term (b -> b)
convRegex
    = convStringValue "illegal regular expression"

convRegexSeq :: (String -> Maybe (b -> b)) -> OptInfo -> Term (b -> b)
convRegexSeq
    = convStringSeqValue "illegal regular expression"

setFindRegex :: (Regex -> FindExpr) -> String -> Maybe (Env -> Env)
setFindRegex _ ""
    = return id
setFindRegex constr s
    = fmap setFind $ checkRegex s
    where
      setFind re e
          =  e { theUserFindExpr = andExpr (theUserFindExpr e) (constr re) }

addFindExpr :: FindExpr -> (Env -> Env)
addFindExpr fe e
    =  e { theUserFindExpr = andExpr (theUserFindExpr e) fe }

-- ----------------------------------------


