module Main
where

import Control.Applicative
import Control.Monad.RWSErrorIO

import Data.Char                ( isDigit )
import Data.List                ( nub )

import System.Exit

import System.DirTree.Core
import System.DirTree.Hash
import System.DirTree.FileSystem
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
      { termName = "fgs2"
      , version  = "0.1.5.0"
      }

oAll :: Term (Env -> Env)
oAll = oVerbose
       <.> oQuiet
       <.> oBackup
       <.> oReadUtf8   <.> oWriteUtf8 <.> oUtf8
       <.> oMatchDir   <.> oNotMatchDir
       <.> oMatchOwner <.> oNotMatchOwner
       <.> oMatchGroup <.> oNotMatchGroup
       <.> oMatchName  <.> oNotMatchName
       <.> oMatchPath  <.> oNotMatchPath
       <.> oMatchExt   <.> oNotMatchExt
       <.> oMinLevel   <.> oMaxLevel
       <.> oSpecial
       <.> oScan
       <.> oFollowSymLink
       <.> oTypes
       <.> oFind
       <.> oGrep
       <.> oSed
       <.> oHash <.> oUpdateHash <.> oSha1 <.> oMd5

-- ----------------------------------------
--
-- the option definitions

oVerbose :: Term (Env -> Env)
oVerbose
    = convFlag setVerbose
      $ (optInfo ["verbose"])
            { optDoc = "Turn on trace output." }
    where
      setVerbose True  e = e { theTraceFlag   = True
                             , theWarningFlag = True
                             }
      setVerbose False e = e

oQuiet :: Term (Env -> Env)
oQuiet
    = convFlag setQuiet
      $ (optInfo ["quiet"])
            { optDoc = "Turn off warnings and trace output." }
    where
      setQuiet True  e = e { theTraceFlag   = False
                           , theWarningFlag = False
                           }
      setQuiet False e = e

oFollowSymLink :: Term (Env -> Env)
oFollowSymLink
    = convFlag setSymLink
      $ (optInfo ["follow-symlink"])
            { optDoc = "Follow symbolic links." }
    where
      setSymLink True  e = e { theFollowSymlink = True }
      setSymLink False e = e

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
            { optDoc = unwords
                       [ "File contents are assumed to be utf8 encoded"
                       , "when reading files in grep and sed commands."
                       ]
            }
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
            { optName = "TYPES"
            , optDoc  = unwords
                        [ "Test whether entry is one of the types specified in TYPES."
                        , "Every char in TYPES stands for a type,"
                        , "'d' stands for directory, 'f' for file, 'l' for symbolic link,"
                        , "'c' for character device. 'b' for block device,"
                        , "'s' for socket, 'p' for named pipe."
                        ]
            }
    where
      setTypes s
          | null s
              = Just id
          | all (`elem` "bcdflps") s
              = Just $ addFindExpr typeExpr
          | otherwise
              = Nothing
          where
            c2e 'b'  = HasType IsBlockDev
            c2e 'c'  = HasType IsCharDev
            c2e 'd'  = HasType IsDir
            c2e 'f'  = HasType IsFile
            c2e 'l'  = HasType IsSymLink
            c2e 'p'  = HasType IsNamedPipe
            c2e 's'  = HasType IsSocket
            c2e _    = FFalse

            typeExpr = orExprSeq . map c2e . nub $ s

-- ----------------------------------------

oMinLevel :: Term (Env -> Env)
oMinLevel
    = convStringValue "LEVEL must be a number" setLevel
      $ (optInfo ["min-level"])
            { optName = "LEVEL"
            , optDoc  = unwords
                        [ "Test whether entry is located at least MINLEVEL steps deep in directory tree."
                        , "The root directory has level 0."
                        , "Example: '--min-level=1' excludes entries in the root directory"
                        , "(see also '--max-level')."
                        ]
            }
    where
      setLevel s
          | null s
              = Just id
          | all isDigit s
              = Just $ addFindExpr levelExpr    -- predicate for checking entries
          | otherwise
              = Nothing
          where
            levelExpr = HasFeature "minlevel" $ const $ asks ((>= read s) . theLevel)

oMaxLevel :: Term (Env -> Env)
oMaxLevel
    = convStringValue "LEVEL must be a number" setLevel
      $ (optInfo ["max-level"])
            { optName = "LEVEL"
            , optDoc  = unwords
                        [ "Test whether entry is located at most MAXLEVEL steps deep in directory tree."
                        , "The root directory has level 0."
                        , "Example: '--max-level=1 restricts search to root dir and all child dirs"
                        , "(see also: '--min-level')."
                        ]
            }
    where
      setLevel s
          | null s
              = Just id
          | all isDigit s
              = Just $ addDirExpr levelExpr     -- predicate for recursing directories
          | otherwise
              = Nothing
          where
            levelExpr = HasFeature "maxlevel" $ const $ asks ((< read s) . theLevel)

-- ----------------------------------------

oMatchDir :: Term (Env -> Env)
oMatchDir
    = convRegexSeq setMatchDir
      $ (optInfo ["dir-path"])
            { optName = "REGEXP"
            , optDoc  = unwords
                        [ "Test whether a dir pathname matches REGEXP and has to be processed."
                        , "The subdirectories to recurse into can be selected with this options."
                        , "The REGEX must be a sequence of regular expressions separated by '/'-es"
                        , "and must start with './'"
                        ]
            }
    where
      setMatchDir = setDirRegex (matchPathPred id)

oNotMatchDir :: Term (Env -> Env)
oNotMatchDir
    = convRegexSeq setMatchDir
      $ (optInfo ["not-dir-path"])
            { optName = "REGEXP"
            , optDoc  = unwords
                        [ "Test whether a dir pathname does not match REGEXP and has to be skipped"
                        , "when recursing into the dir tree"
                        , "(negation of --dir option)."
                        ]
            }
    where
      setMatchDir = setDirRegex (matchPathPred not)

matchPathPred :: (Bool -> Bool) -> [Regex] -> (FindExpr, FindExpr)
matchPathPred bf re
    = (fex True, fex False)
    where
      fex b
          = HasFeature "matchPathPred" $
            \ f -> do p <- pathName f
                      return . bf $ matchPath b re p

-- ----------------------------------------

oMatchOwner :: Term (Env -> Env)
oMatchOwner
    = convRegexSeq setMatchName
      $ (optInfo ["owner"])
            { optName = "REGEXP"
            , optDoc  = "Test whether owner of entry matches REGEXP."
            }
    where
      setMatchName = setFindRegex $ hasFSPred "owner" id getFileOwner

oNotMatchOwner :: Term (Env -> Env)
oNotMatchOwner
    = convRegexSeq setMatchName
      $ (optInfo ["not-owner"])
            { optName = "REGEXP"
            , optDoc  = "Test whether owner of entry does not match REGEXP."
            }
    where
      setMatchName = setFindRegex $ hasFSPred "not-owner" not getFileOwner

oMatchGroup :: Term (Env -> Env)
oMatchGroup
    = convRegexSeq setMatchName
      $ (optInfo ["group"])
            { optName = "REGEXP"
            , optDoc  = "Test whether group of entry matches REGEXP."
            }
    where
      setMatchName = setFindRegex $ hasFSPred "group" id getFileGroup

oNotMatchGroup :: Term (Env -> Env)
oNotMatchGroup
    = convRegexSeq setMatchName
      $ (optInfo ["not-group"])
            { optName = "REGEXP"
            , optDoc  = "Test whether group of entry does not match REGEXP."
            }
    where
      setMatchName = setFindRegex $ hasFSPred "not-group" not getFileGroup

hasFSPred :: String -> (Bool -> Bool) -> (FilePath -> Cmd String) -> Regex -> FindExpr
hasFSPred s bf getf re
    = HasFeature s $ \ f -> (bf . matchRE re) <$> getf f

-- ----------------------------------------

oMatchName :: Term (Env -> Env)
oMatchName
    = convRegexSeq setMatchName
      $ (optInfo ["name"])
            { optName = "REGEXP"
            , optDoc  = "Test whether file name matches REGEXP."
            }
    where
      setMatchName = setFindRegex MatchRE

oNotMatchName :: Term (Env -> Env)
oNotMatchName
    = convRegexSeq setMatchName
      $ (optInfo ["not-name"])
            { optName = "REGEXP"
            , optDoc  = "Test whether file name does not match REGEXP."
            }
    where
      setMatchName = setFindRegex (NotExpr . MatchRE)

oMatchPath :: Term (Env -> Env)
oMatchPath
    = convRegexSeq setMatchPath
      $ (optInfo ["path"])
            { optName = "REGEXP"
            , optDoc  = "Test whether whole path matches REGEXP."
            }
    where
      setMatchPath = setFindRegex MatchPathRE

oNotMatchPath :: Term (Env -> Env)
oNotMatchPath
    = convRegexSeq setMatchPath
      $ (optInfo ["not-path"])
            { optName = "REGEXP"
            , optDoc  = "Test whether whole path does not match REGEXP."
            }
    where
      setMatchPath = setFindRegex (NotExpr . MatchPathRE)

oMatchExt :: Term (Env -> Env)
oMatchExt
    = convRegexSeq setMatchExt
      $ (optInfo ["ext"])
            { optName = "REGEXP"
            , optDoc  = "Test whether whole path matches REGEXP."
            }
    where
      setMatchExt = setFindRegex MatchExtRE

oNotMatchExt :: Term (Env -> Env)
oNotMatchExt
    = convRegexSeq setMatchExt
      $ (optInfo ["not-ext"])
            { optName = "REGEXP"
            , optDoc  = "Test whether whole path does not match REGEXP."
            }
    where
      setMatchExt = setFindRegex (NotExpr . MatchExtRE)

oSpecial :: Term (Env -> Env)
oSpecial
    = convStringSeqValue "illegal special function" setSpecial
      $ (optInfo ["special"])
            { optName = "SPECIAL-PRED"
            , optDoc  = unwords
                        [ "Test whether a special buildin predicate holds."
                        , "Currently buildin predicates are:"
                        , "'{boring-file}',"
                        , "'{not-boring-file}',"
                        , "'{uppercase-img-file}' and"
                        , "'{not-uppercase-img-file}'."
                        ]
            }
    where
      setSpecial ""
          = return id
      setSpecial s
          = fmap addFindExpr $ checkSpecial s
          where
            checkSpecial "{boring-file}"            = fe isBoringFile
            checkSpecial "{not-boring-file}"        = fn isBoringFile
            checkSpecial "{uppercase-img-file}"     = fe isUpperCaseImgFile
            checkSpecial "{not-uppercase-img-file}" = fn isUpperCaseImgFile
            checkSpecial "{is-album-file}"          = fe isAlbumFile
            checkSpecial "{not-is-album-file}"      = fn isAlbumFile
            checkSpecial _                          = Nothing

            fe p = Just . HasFeature ("special " ++ s) $ p
            fn p = Just . HasFeature ("special " ++ s) $ p >=> return . not

oScan :: Term (Env -> Env)
oScan
    = convStringSeqValue "illegal scan function or regexp" setScan
      $ (optInfo ["scan"])
            { optName = "SCAN-FCT-or-REGEXP"
            , optDoc  = unwords
                        [ "Test whether file contents has some feature given by SCAN-FCT."
                        , "SCAN-FCT may have the following values:"
                        , "'{ascii}', '{latin1}', '{latin1-ascii}', '{unicode}',"
                        , "'{unicode-latin1}', '{utf8}', '{utf8-ascii}', '{trailing-ws}', '{tabs}'"
                        , "or it may be a regular expression like in --grep option."
                        , "\n"
                        , "Meaning:"
                        , "'{latin1-ascii}': Latin1 with some none ascii chars,"
                        , "'{utf8}': Utf8 with some utf8 multi byte chars,"
                        , "'{unicode-latin1}': Unicode with some none latin1 chars."
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
            checkScan "{ascii}"          = fe isAsciiText
            checkScan "{latin1}"         = fe isLatin1Text
            checkScan "{latin1-ascii}"   = fe containsLatin1
            checkScan "{unicode}"        = fe isUnicodeText
            checkScan "{unicode-latin1}" = fe containsNoneLatin1
            checkScan "{utf8}"           = fe isUtf8
            checkScan "{utf8-ascii}"     = fe isUtfText
            checkScan "{trailing-ws}"    = fe hasTrailingWSLine
            checkScan "{tabs}"           = fe containsTabs
            checkScan s'                 = fmap grp $ checkContextRegex s'

            fe x                         = Just $ HasCont s $ (return . x)

            grp re                       = HasCont ("scan " ++ s) $ (return . any (matchRE re) . lines)

-- ----------------------------------------
--
-- the action opions

oFind :: Term (Env -> Env)
oFind
    = convDefaultStringValue "illegal print command" setFind "{path}"
      $ (optInfo ["print"])
            { optName = "FORMAT"
            , optDoc  = unwords
                        [ "Print matching file paths."
                        , "FORMAT controls output."
                        , "It can be an arbitrary string containing format directives."
                        , "Following format directives are implemented:"
                        , "'{path}', '{name}', '{ext}', '{dir}' and '{base}'," 
                        , "'{lc-path}', '{lc-name}', '{lc-ext}', '{lc-dir}' and '{lc-base}'."
                        , "With the 'lc'-formats the strings are converted to lowercase."
                        , "The --print option with FORMAT can be used to generate scripts"
                        , "for manipulating the dirtree entries by e.g. shell commands."
                        ]
            }
    where
      setFind ""
          = return id
      setFind s
          = return $ \ e -> e {theProcessor = genFindProcessor s}

oGrep :: Term (Env -> Env)
oGrep
    = convStringValue "illegal grep function or regexp" setGrep
      $ (optInfo ["grep"])
            { optName = "GREP-FCT-or-REGEXP"
            , optDoc  = unwords
                        [ "Find lines in all selected files matching REGEXP."
                        , "Context specs (^, $, \\<, \\>) like in egrep are allowed."
                        , "The arguments in --scan may also be used here as GREP-FCT,"
                        , "e.g. '--grep {tabs}' lists all lines containing tabs,"
                        ,"'--grep {latin1-ascii}' lists all lines containing none ascii latin1 chars."
                        ]
            }
    where
      setGrep ""
          = return id
      setGrep s
          = fmap setG $ checkGrep s
          where
            checkGrep "{ascii}"          = Just isAsciiText
            checkGrep "{latin1}"         = Just isLatin1Text
            checkGrep "{latin1-ascii}"   = Just containsLatin1
            checkGrep "{unicode}"        = Just isUnicodeText
            checkGrep "{unicode-latin1}" = Just containsNoneLatin1
            checkGrep "{utf8}"           = Just isUtf8
            checkGrep "{utf8-ascii}"     = Just isUtfText
            checkGrep "{trailing-ws}"    = Just hasTrailingWSLine
            checkGrep "{tabs}"           = Just containsTabs
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
            , optDoc  = unwords
                        [ "Edit files with given edit function."
                        , "currently supported edit functions are:"
                        , "'{umlauts-to-ascii}', '{umlauts-to-tex}',"
                        , "'{html-to-ascii}', '{haskell-to-ascii}', '{tcl-to-ascii}',"
                        , "'{no-trailing-ws}', '{no-tabs}'."
                        ]
            }
    where
      setSed ""
          = return id
      setSed s
          = fmap setS $ checkSed s
          where
            checkSed "{umlauts-to-ascii}" = Just substUmlauts
            checkSed "{umlauts-to-tex}"   = Just substUmlautsTex
            checkSed "{html-to-ascii}"    = Just substXhtmlChars
            checkSed "{haskell-to-ascii}" = Just substToAsciiHaskell
            checkSed "{tcl-to-ascii}"     = Just substLatin1Tcl
            checkSed "{no-trailing-ws}"   = Just removeTrailingWS
            checkSed "{no-tabs}"          = Just removeTabs
            checkSed _                    = Nothing

            setS f e
                = e { theProcessor = genSedProcessor
                    , theSedFct  = f
                    }

-- ----------------------------------------
--
-- checksum options

oHash :: Term (Env -> Env)
oHash
    = convFlag setHash
      $ (optInfo ["check-hashes"])
            { optDoc = unwords
                       [ "Compute, check and update hashes in checksum file."
                       , "Default hash function is 'sha1',"
                       , "default hash dictionary is '.sha1'"
                       ]
            }
      where
        setHash True  e = addFindExpr (HasType IsFile) $
                          e { theProcessor = genChecksumProcessor
                            }
        setHash False e = e

oUpdateHash :: Term (Env -> Env)
oUpdateHash
    = convFlag setUpdateHash
      $ (optInfo ["update-hashes"])
            { optDoc = unwords
                       [ "Update hash dictionaries when checking hashes"
                       , "(see '--check-hashes' for details)."
                       ]
            }
    where
      setUpdateHash True  e = e { theHashUpdate = True }
      setUpdateHash False e = e

oSha1 :: Term (Env -> Env)
oSha1 = oHashFct "sha1" sha1Hash

oMd5 :: Term (Env -> Env)
oMd5 = oHashFct "md5" md5Hash

oHashFct :: String -> HashFct -> Term (Env -> Env)
oHashFct name fct
    = convDefaultStringValue "illegal checksum file name" setChecksumFile ("." ++ name)
      $ (optInfo [name])
            { optName = "HASH-FILE"
            , optDoc  = unwords
                        [ "Set hash function to"
                        , name
                        , "and use"
                        , "HASH-FILE for the dictionary of hashes."
                        ]
            }
    where
      setChecksumFile ""
          = return id
      setChecksumFile f
          = fmap setS $ checkName f

      setS f e
          = e { theHashFct      = fct
              , theChecksumFile = f
              }

      checkName s
          | all (/= '/') s = Just s
          | otherwise      = Nothing

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
    = fmap (addFindExpr . constr) $ checkRegex s

addFindExpr :: FindExpr -> (Env -> Env)
addFindExpr fe e
    =  e { theUserFindExpr = andExpr (theUserFindExpr e) fe }


setDirRegex :: ([Regex] -> (FindExpr, FindExpr)) -> String -> Maybe (Env -> Env)
setDirRegex _ ""
    = return id
setDirRegex constr s
    = fmap (uncurry addExpr . constr) $ checkPathRegex s
    where
      addExpr de fe = addDirExpr de . addFindExpr fe

addDirExpr :: FindExpr -> (Env -> Env)
addDirExpr fe e
    =  e { theDirFindExpr = andExpr (theDirFindExpr e) fe }

-- ----------------------------------------


