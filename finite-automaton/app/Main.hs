{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Automaton.Types ( Q, NFA', DFA', Input, Token)
import           Automaton.Run ( acceptDFA
                      , acceptNFA
                      , scanDFA''
                      , scanNFA''
                      )
import           Automaton.Transform ( convertNFAtoDFA
                      , minDFA'
                      , removeSetsDFA
                      , removeSetsDFAMin
                      , addStateAttr
                      , mapAttr
                      , mapSetAttr
                      )
import           Automaton.GenDot ( GenDotAttr(..)
                        , genDotNFA
                        , genDotDFA
                        )
import           Automaton.GenCode ( genCodeNFA'
                                   , genCodeDFA'
                                   )
import           Automaton.ScanSpec (PrioLabel(thePrioLabel), mkPrio0, scanSpecToNFA)

import           Control.Monad.RWSErrorIO
import qualified Control.Monad.RWSErrorIO as RWS(exec)

import           Data.List ( intercalate )
import           Data.Set.Simple ( Set )
import           Data.Maybe

import           Regex.Core ( Regex, statesRE, reToNFA)
import           Regex.Parse ( parseRegex )

import           System.Console.CmdTheLine
import           System.Console.CmdTheLine.Utils
import           System.FilePath ((</>), replaceExtension, takeFileName, dropExtension)
import           System.Exit

import           Text.Read (readMaybe)

-- imports for examples
import           Automaton.Types (mkDFA, mkNFA, Automaton(..))
import           Automaton.Transform ( convertDFAtoNFA
                      , addAttr
                      , minDFA
                      )
import           Automaton.GenCode ( genCodeDFA, genCodeNFA )
import           Data.Set.Simple (empty, fromList, singleton)

-- import           Data.Map.Simple
-- import           Data.Set.Simple
-- import           Data.Either

import Examples

-- ----------------------------------------

main :: IO ()
main =
  run (runAuto <$> oAll, autoInfo)

runAuto :: (Env -> Env) -> IO ()
runAuto setOpts
  = do res <- evalAction doIt env initState
       maybe exitFailure return res
  where
    env = setOpts initEnv

-- ----------------------------------------

autoInfo :: TermInfo
autoInfo
  = defTI
    { termName = "finite-automaton"
    , version  = "0.1.0.0"
    }

-- ----------------------------------------

data Env
    = Env
      { theProgName      :: String
      , theTraceFlag     :: Bool
      , theWarningFlag   :: Bool
      , theErrorFlag     :: Bool
      , theStdErrFlag    :: Bool
      , theName          :: String
      , theInpSpec       :: InpSpec
      , theDotDir        :: String
      , thePngDir        :: String
      , theCodeDir       :: String
      , genNFA           :: (Bool, Bool)
      , genDFASet        :: (Bool, Bool)
      , genDFA           :: (Bool, Bool)
      , genDFAMinSet     :: (Bool, Bool)
      , genDFAMin        :: (Bool, Bool)
      , theInputLimit    :: Maybe Int
      , theRegexLimit    :: Maybe Int
      , acceptWithNFA    :: Maybe String
      , acceptWithDFA    :: Maybe String
      , acceptWithDFAMin :: Maybe String
      , scanWithNFA      :: Maybe (Cmd String)
      , scanWithDFA      :: Maybe (Cmd String)
      , scanWithDFAMin   :: Maybe (Cmd String)
      }

data InpSpec
  = RegexSpec (Cmd String)
  | ScannSpec (Cmd String)
  | ExampleA  (NFA' Q (Set Q, (PrioLabel String, ())))
  | ExampleS  [(String, String)]
  | ExampleR  String
    
instance Config Env where
    traceOn   = theTraceFlag
    warningOn = theWarningFlag
    errorOn   = theErrorFlag
    stderrOn  = theStdErrFlag

initEnv :: Env
initEnv
  = Env
    { theProgName      = "finite-automaton"
    , theTraceFlag     = False
    , theWarningFlag   = True
    , theErrorFlag     = True
    , theStdErrFlag    = True
    , theName          = ""
    , theInpSpec       = RegexSpec (abort "no regex given")
    , theDotDir        = "."
    , thePngDir        = "."
    , theCodeDir       = "."
    , genNFA           = (True, True)
    , genDFASet        = (True, True)
    , genDFA           = (True, True)
    , genDFAMinSet     = (True, True)
    , genDFAMin        = (True, True)
    , theInputLimit    = Nothing
    , theRegexLimit    = Nothing
    , acceptWithNFA    = Nothing
    , acceptWithDFA    = Nothing
    , acceptWithDFAMin = Nothing
    , scanWithNFA      = Nothing
    , scanWithDFA      = Nothing
    , scanWithDFAMin   = Nothing
    }
    
-- ----------------------------------------

oAll :: Term (Env -> Env)
oAll
  = oVerbose
    <.> oQuiet
    <.> oName
    <.> oRegex         <.> oRegexFile
    <.> oScanSpec      <.> oScanSpecFile
    <.> oExample
    <.> oAcceptWithNFA <.> oAcceptWithDFA <.> oAcceptWithDFAMin
    <.> oScanWithNFA   <.> oScanWithDFA   <.> oScanWithDFAMin
    <.> oDotDir        <.> oPngDir        <.> oCodeDir
    <.> oInputLimit
    
-- ----------------------------------------
--
-- the option definitions

oVerbose :: Term (Env -> Env)
oVerbose
  = convFlag setVerbose
    $ (optInfo ["v", "verbose"])
            { optDoc = "Turn on trace output." }
  where
    setVerbose True  e = e { theTraceFlag   = True
                           , theWarningFlag = True
                           }
    setVerbose False e = e

oQuiet :: Term (Env -> Env)
oQuiet
  = convFlag setQuiet
    $ (optInfo ["q", "quiet"])
            { optDoc = "Turn off warnings and trace output." }
  where
    setQuiet True  e = e { theTraceFlag   = False
                         , theWarningFlag = False
                         }
    setQuiet False e = e

oName :: Term (Env -> Env)
oName
  = convStringValue "name expected" (Just . setName)
    $ (optInfo ["n", "name"])
            { optName = "NAME"
            , optDoc = "The name of the finite automaton."
            }
  where
    setName "" = id
    setName n  = \ e -> e { theName = n }

oDotDir, oPngDir, oCodeDir :: Term (Env -> Env)
oDotDir  = oDir "dot"  ".dot" (\ n e -> e { theDotDir  = n})
oPngDir  = oDir "png"  ".png" (\ n e -> e { thePngDir  = n})
oCodeDir = oDir "code" ".hs"  (\ n e -> e { theCodeDir = n}) 

oDir :: String -> String -> (String -> Env -> Env) -> Term (Env -> Env)
oDir s1 s2 set
  = fmap set . value . opt "."
    $ (optInfo [s1 ++ "-dir"])
            { optName = "DIR"
            , optDoc = "Output directory for " ++ s2 ++ " source code files."
            }

oRegex :: Term (Env -> Env)
oRegex
  = fmap setRegex . value . opt ""
    $ (optInfo ["r", "regex"])
            { optName = "REGEX"
            , optDoc  = "The regular expression to be converted into an automaton."
            }
  where
    setRegex "" = id
    setRegex r  = \ e -> e { theInpSpec = RegexSpec (return r) }

oScanSpec :: Term (Env -> Env)
oScanSpec
  = fmap setScan . value . opt ""
    $ (optInfo ["s", "scan-spec"])
            { optName = "SCANNER-SPEC"
            , optDoc  = "The scanner spec to be converted into an automaton."
            }
  where
    setScan "" = id
    setScan r  = \ e -> e { theInpSpec = ScannSpec (return r) }

oRegexFile :: Term (Env -> Env)
oRegexFile
  = convStringValue "file name expected" (Just . setRegex)
    $ (optInfo ["regex-file"])
            { optName= "INPUT-FILE"
            , optDoc  = unwords
                        [ "Regex is read from a file,"
                        , "not from the \"--regex\" command line argument,"
                        , "if INPUT-FILE=\"-\", the regex is read from stdin."
                        , "If no \"--name\" option is specified and input is"
                        , "read from a file, the basename of the file is taken as"
                        , "name of the automaton."
                        ]
            }
  where
    setRegex ""  = id
    setRegex "-" = \ e -> e { theInpSpec = RegexSpec readFromStdin }
    setRegex fn  = \ e -> e { theInpSpec = RegexSpec (readFromFile fn)
                            , theName    = fnToName  (theName e) fn
                            }
    fnToName "" fn = dropExtension . takeFileName $ fn
    fnToName n  _  = n  -- name already set

oScanSpecFile :: Term (Env -> Env)
oScanSpecFile
  = convStringValue "file name expected" (Just . setScan)
    $ (optInfo ["scan-spec-file"])
            { optName= "INPUT-FILE"
            , optDoc  = unwords
                        [ "The scanner spec is read from a file,"
                        , "not from the \"--scan-spec\" command line argument,"
                        , "if INPUT-FILE=\"-\", the spec is read from stdin."
                        , "If no \"--name\" option is specified and input is"
                        , "read from a file, the basename of the file is taken as"
                        , "name of the automaton."
                        ]
            }
  where
    setScan ""  = id
    setScan "-" = \ e -> e { theInpSpec = ScannSpec readFromStdin }
    setScan fn  = \ e -> e { theInpSpec = ScannSpec (readFromFile fn)
                           , theName    = fnToName  (theName e) fn
                           }
    fnToName "" fn = dropExtension . takeFileName $ fn
    fnToName n  _  = n  -- name already set

oExample :: Term (Env -> Env)
oExample
  = convStringValue "example automaton not found" setEx
    $ (optInfo ["example"])
            { optName= "EXAMPLE"
            , optDoc  = unwords $
                        [ "Select a builtin example,"
                        , "examples are"
                        , intercalate ", "
                          ( map (show . fst) nfaExamples
                            ++ map (show . fst) reExamples
                            ++ map (show . fst) scanExamples
                          )
                        , "."
                        ]
            }
  where
    setEx ""  = Just id
    setEx en
      | isJust nfa
          = Just $
            \ e -> e { theInpSpec = ExampleA (fromJust nfa)
                     , theName    = en
                     }
      | isJust scn
          = Just $
            \ e -> e { theInpSpec = ExampleS (fromJust scn)
                     , theName    = en
                     }
      | isJust rex
          = Just $
            \ e -> e { theInpSpec = ExampleR (fromJust rex)
                     , theName    = en
                     }
      | otherwise
          = Nothing
      where
        nfa = lookup en nfaExamples
        scn = lookup en scanExamples
        rex = lookup en reExamples

oInputLimit :: Term (Env -> Env)
oInputLimit
  = convStringValue "number >= 1 expected" setLimit
    $ (optInfo ["max-regex-length"])
            { optName = "NUMBER"
            , optDoc  = unwords
                        [ "The maximum length of the string representing the regular expression,"
                        , "and the maximum complexity of the resulting regular expression,"
                        , "default is \"0\" for no limit."
                        ]
            }
  where
    setLimit ""
      = Just id
    setLimit l
      = fmap f $ readMaybe l
      where
        f :: Int -> (Env -> Env)
        f i = \ e -> e { theInputLimit
                           = if i > 0
                             then Just i
                             else Nothing
                       , theRegexLimit
                           = if i > 0
                             then Just i
                             else Nothing
                       }


oAcceptWithNFA, oAcceptWithDFA, oAcceptWithDFAMin :: Term (Env -> Env)

oAcceptWithNFA
  = oAcceptWith "nfa" "NFA" (\ v e -> e { acceptWithNFA = v })
oAcceptWithDFA
  = oAcceptWith "dfa" "DFA" (\ v e -> e { acceptWithDFA = v })
oAcceptWithDFAMin
  = oAcceptWith "dfamin" "minimal DFA" (\ v e -> e { acceptWithDFAMin = v })

oAcceptWith :: String -> String -> (Maybe String -> Env -> Env) -> Term (Env -> Env)
oAcceptWith s1 s2 set
  = convStringValue "no input given" (Just . setAccept)
    $ (optInfo ["accept-" ++ s1])
            { optName= "WORD"
            , optDoc  = "Input for a word test with " ++ s2 ++ " constructed from a regex."
            }
  where
    setAccept w = (set (Just w)) . resetGenDot


oScanWithNFA, oScanWithDFA, oScanWithDFAMin :: Term (Env -> Env)

oScanWithNFA
  = oScanWith "nfa" "NFA" (\ v e -> e { scanWithNFA = v })
oScanWithDFA
  = oScanWith "dfa" "DFA" (\ v e -> e { scanWithDFA = v })
oScanWithDFAMin
  = oScanWith "dfamin" "minimal DFA" (\ v e -> e { scanWithDFAMin = v })

oScanWith :: String -> String -> (Maybe (Cmd String) -> Env -> Env) -> Term (Env -> Env)
oScanWith s1 s2 set
  = convStringValue "no input given" (Just . setScan)
    $ (optInfo ["scan-" ++ s1])
            { optName= "WORD"
            , optDoc  = "Input for a scanner run with " ++ s2 ++ " constructed from a regex."
            }
  where
    setScan w = (set (Just $ return w)) . resetGenDot

resetGenDot :: Env -> Env
resetGenDot e
  = e { genNFA           = (False, False)
      , genDFASet        = (False, False)
      , genDFA           = (False, False)
      , genDFAMinSet     = (False, False)
      , genDFAMin        = (False, False)
      }
    
-- ----------------------------------------

type State = ()

initState :: State
initState = ()

-- ----------------------------------------

type Cmd = Action Env State

doIt :: Cmd ()
doIt
    = withName $
      \ n -> do trc $ "start processing automaton " ++ show n
                processRegex
                trc $ "end processing automaton "   ++ show n

processRegex :: Cmd ()
processRegex
  = processTheInpSpec     >>= tee nfaToDot
                          >>= tee runAcceptNFA
                          >>= tee runScanNFA
    >>= nfaToDFAset       >>= tee dfaSetToDot
    >>= dfaSetToDFA       >>= tee dfaToDot
                          >>= tee runAcceptDFA
                          >>= tee runScanDFA
    >>= dfaToDFAMinSet    >>= tee dfaMinSetToDot
    >>= dfaMinSetToDFAMin >>= tee dfaMinToDot
                          >>= tee runAcceptDFAMin
                          >>= tee runScanDFAMin
    >>= (return . const ())

processTheInpSpec :: Cmd (NFA' Q (Set Q, (PrioLabel String, ())))
processTheInpSpec
  = do inp <- asks theInpSpec
       case inp of
        RegexSpec cmd
          -> processRE cmd
        ScannSpec cmd
          -> processSS cmd
        ExampleA nfa
          -> return nfa
        ExampleS spec
          -> either abort return $
             scanSpecToNFA spec
        ExampleR rex
          -> processRE (return rex)
  where
    processRE cmd
      = cmd
        >>= checkInputLimit >>= stringToRegex
        >>= checkRegexLimit >>= regexToNFA

    processSS cmd
      = cmd
        >>= scannerToNFA

       
checkInputLimit :: String -> Cmd String
checkInputLimit xs
  = do l <- asks theInputLimit
       maybe
         (return xs)
         (\ mx ->
           do trc $ "check input length, limit is " ++ show mx
              checkLength mx
         ) l
  where
    checkLength mx
      | null zs   = return ys
      | otherwise = abort $ "given input to complex, max length is " ++ show mx
      where
        (ys, zs) = splitAt mx xs

checkRegexLimit :: Regex -> Cmd Regex
checkRegexLimit re
  = do l <- asks theRegexLimit
       maybe
         (return re)
         (\ mx ->
           do trc $ "check regex size, limit is " ++ show mx
              checkSize mx
         ) l
  where
    checkSize mx
      | n <= mx   = return re
      | otherwise = abort
                    $ "given regex to complex, size is "
                      ++ show n
                      ++ ", max size is "
                      ++ show mx
      where
        n = statesRE re

        
stringToRegex :: String -> Cmd Regex
stringToRegex xs
  = do trc $ "parse regex: " ++ show xs
       either
         abort
         (\ re -> do trc $ "parsed regex " ++ show re
                     return re
         ) $ parseRegex xs

regexToNFA :: Regex -> Cmd (NFA' Q (Set Q, (PrioLabel String, ())))
regexToNFA re
  = do trc $ "transform regex into NFA"
       return ( mapSetAttr
                . addStateAttr
                . addAttr (const mkPrio0)
                . reToNFA
                $ re
              )

scannerToNFA :: String ->  Cmd (NFA' Q (Set Q, (PrioLabel String, ())))
scannerToNFA xs
  = do trc $ "transform scanner spec into NFA"
       spec <- maybe
               (abort "syntax error in scanner spec")
               return $ readSpec xs
       either abort return $
         scanSpecToNFA spec
  where
    readSpec :: String -> Maybe [(String, String)]
    readSpec = readMaybe
    
nfaToDFAset :: (Ord a, Monoid a1) => NFA' Q (a, a1) -> Cmd (DFA' Q (Set a, a1))
nfaToDFAset a
  = do trc $ "transform NFA into DFA with state sets as labels"
       return ( removeSetsDFA       -- state sets -> numbers
                . convertNFAtoDFA
                . mapSetAttr        -- make attr a monoid with `union`
                $ a
              )

dfaSetToDFA :: DFA' Q (a, a1) -> Cmd (DFA' Q (Q, a1))
dfaSetToDFA a
  = do trc $ "rename sets of states in DFA into simple states"
       return ( addStateAttr        -- add state numbers
                . mapAttr snd       -- remove the state set attribute
                $ a
              )

dfaToDFAMinSet :: (Ord a, Monoid a1, Ord a2) =>
                  DFA' Q (a, (PrioLabel a2 , a1)) -> Cmd (DFA' Q (Set a, (PrioLabel a2, a1)))
dfaToDFAMinSet a
  = do trc $ "minimize DFA into min DFA with sets as states"
       return ( removeSetsDFAMin    -- state sets -> numbers
                . minA
                . mapSetAttr        -- make attr a monoid with `union`
                $ a
              )
  where
    minA a@(A {_attr = f})
      = minDFA' part a
      where
        part q = fmap fst . thePrioLabel . fst . snd $ f q

dfaMinSetToDFAMin :: DFA' Q (a, a1) -> Cmd (DFA' Q (Q, a1))
dfaMinSetToDFAMin a
  = do trc $ "rename set of equiv states in min DFA into simple states"
       return ( addStateAttr        -- add state numbers
                . mapAttr snd       -- remove the state set attribute
                $ a
              )

-- --------------------

runAcceptNFA :: (Ord q) => NFA' q a -> Cmd ()
runAcceptNFA = runAcceptAutomaton acceptNFA acceptWithNFA

runAcceptDFA :: (Ord q) => DFA' q a -> Cmd ()
runAcceptDFA = runAcceptAutomaton acceptDFA acceptWithDFA
         
runAcceptDFAMin :: (Ord q) => DFA' q a -> Cmd ()
runAcceptDFAMin = runAcceptAutomaton acceptDFA acceptWithDFAMin

runAcceptAutomaton :: (Ord q) =>
                      (Automaton delta q a -> Input -> Bool) ->
                      (Env -> Maybe String) ->
                      Automaton delta q a   ->
                      Cmd ()
runAcceptAutomaton acceptFct acceptWith a
  = do word <- asks acceptWith
       maybe
         (return ())
         (\ w -> writeToStdout . show $ acceptFct a w)
         word

runScanNFA :: (Ord q, Show q, Monoid a, GenDotAttr a) => NFA' q a -> Cmd ()
runScanNFA = runScanAutomaton scanNFA'' scanWithNFA

runScanDFA :: (Ord q, Show q, GenDotAttr a) => DFA' q a -> Cmd ()
runScanDFA = runScanAutomaton scanDFA'' scanWithDFA

runScanDFAMin :: (Ord q, Show q, GenDotAttr a) => DFA' q a -> Cmd ()
runScanDFAMin = runScanAutomaton scanDFA'' scanWithDFAMin

runScanAutomaton :: (Ord q, GenDotAttr a) =>
                    (Automaton delta q a -> Input -> ([(a, Token)], Input)) ->
                    (Env -> Maybe (Cmd String)) ->
                    Automaton delta q a   ->
                    Cmd ()
runScanAutomaton scanFct scanWith a
  = do mc <- asks scanWith
       maybe
         (return ())
         (\ inp ->
           do w <- inp
              writeToStdout $
                toOut (scanFct a w)
         )
         mc
  where
    toOut (ts, rest)
      = unlines $
        map (\ (a, t) ->
              "(" ++ genDotAttr' a ++ ", " ++ show t ++ ")"
            ) ts
        ++ (if null rest
            then []
            else ["scanner error: remaining input is " ++ show rest]
           )                 
        
-- --------------------

whenFlag :: (Env -> Bool) -> Cmd () -> Cmd ()
whenFlag f c
  = do b <- asks f
       when b c

whenFlagN :: (Env -> Bool) -> (String -> Cmd ()) -> Cmd ()
whenFlagN f c
  = whenFlag f $ withName c
                                 
withName :: (String -> Cmd a) -> Cmd a
withName c
  = asks (getDef . theName) >>= c
  where
    getDef "" = "unknown"
    getDef n  = n
    
tee :: (a -> Cmd ()) -> a -> Cmd a
tee cmd a
  = cmd a >> return a

nfaToDot :: (Show a, GenDotAttr a) => NFA' Q a-> Cmd ()
nfaToDot a
  = do whenFlagN (fst . genNFA) $
         \ n -> writeDot  (n ++ ".nfa.dot") (genDotNFA n a)

       whenFlagN (snd . genNFA) $
         \ n -> writeCode (n ++ "_nfa.hs") (genCodeNFA' "Q" "(Q, ())" n a)
            
dfaSetToDot :: (Show a, GenDotAttr a) => DFA' Q a -> Cmd ()
dfaSetToDot a
  = do whenFlagN (fst . genDFASet) $
         \ n -> writeDot  (n ++ ".dfa.set.dot") (genDotDFA n a)

       whenFlagN (snd . genDFASet) $
         \ n -> writeCode (n ++ "_dfa_set.hs") (genCodeDFA' "Q" "(Set Q, ())" n a)

dfaToDot :: (Show a, GenDotAttr a) => DFA' Q a-> Cmd ()
dfaToDot a
  = do whenFlagN (fst . genDFA) $
         \ n -> writeDot  (n ++ ".dfa.dot") (genDotDFA n a)

       whenFlagN (snd . genDFASet) $
         \ n -> writeCode (n ++ "_dfa.hs") (genCodeDFA' "Q" "(Q, ())" n a)

dfaMinSetToDot :: (Show a, GenDotAttr a) => DFA' Q a-> Cmd ()
dfaMinSetToDot a
  = do whenFlagN (fst . genDFAMinSet) $
         \ n -> writeDot  (n ++ ".dfa.min.set.dot") (genDotDFA n a)

       whenFlagN (snd . genDFAMinSet) $
         \ n -> writeCode (n ++ "_dfa_min_set.hs") (genCodeDFA' "Q" "a" n a)

dfaMinToDot :: (Show a, GenDotAttr a) => DFA' Q a -> Cmd ()
dfaMinToDot a
  = do whenFlagN (fst . genDFAMin) $
         \ n -> writeDot  (n ++ ".dfa.min.dot") (genDotDFA n a)

       whenFlagN (snd . genDFAMin) $
         \ n -> writeCode (n ++ "_dfa_min.hs") (genCodeDFA' "Q" "a" n a)

writeDot :: FilePath -> String -> Cmd ()
writeDot dotFile s
  = do trc $ "write dot source"
--       trc $ s
       writeToFile theDotDir dotFile s
       dotToPng dotFile

writeCode :: FilePath -> String -> Cmd ()
writeCode out d
  = do trc $ "write haskell source"
--       trc $ d
       writeToFile theCodeDir out d
       return ()

writeToFile :: (Env -> FilePath) -> FilePath -> String -> Cmd ()
writeToFile dir out d
  = do f <- (</> out) <$> asks dir
       trc $ "write file " ++ show f
       io $ writeFile f d
       return ()

writeToStdout :: String -> Cmd ()
writeToStdout xs
  = do -- trc $ "write to stdout " ++ xs
       io $ putStrLn xs
    
dotToPng :: FilePath -> Cmd ()
dotToPng dot
  = do dotFile <- (</> dot) <$> asks theDotDir
       pngFile <- (</> png) <$> asks thePngDir
       trc $ unwords ["convert", dotFile, "to", pngFile]
       RWS.exec "dot" ["-Tpng", "-o" ++ pngFile, dotFile]
  where
    png = replaceExtension dot "png"

readFromStdin :: Cmd String
readFromStdin
  = do trc $ "read contents from stdin"
       io  $ getContents

readFromFile :: FilePath -> Cmd String
readFromFile fn
  = do trc $ "read contents from " ++ show fn
       io $ readFile fn
    
-- ----------------------------------------
-- test stuff
-- ----------------------------------------

dfa1 :: ([Q], [Char], Q, [Q], Q -> Char -> Maybe Q)
dfa1
    = (states, alphabet, q0, f, delta)
    where
    states      = [1..7] ++ [9..13]
    alphabet    = "\n\t" ++ [' '..'~']
    q0          = 1
    f           = [2,3,4,5,6,7,9,11,12,13]

    delta 1 c
        | c == 'i'              = Just 2
        | c `elem` ['a'..'h']
          ||
          c `elem` ['j'..'z']   = Just 4
        | c == '.'              = Just 5
        | c `elem` ['0'..'9']   = Just 7
        | c == '-'              = Just 9
        | c `elem` " \t\n"      = Just 12
        | otherwise             = Just 13

    delta 2 c
        | c == 'f'              = Just 3
        | c `elem` ['a'..'e']
          ||
          c `elem` ['g'..'z']
          ||
          c `elem` ['0'..'9']   = Just 4

    delta 3 c
        | c `elem` ['a'..'z']
          ||
          c `elem` ['0'..'9']   = Just 4

    delta 4 c
        | c `elem` ['a'..'z']
          ||
          c `elem` ['0'..'9']   = Just 4

    delta 5 c
        | c `elem` ['0'..'9']   = Just 6

    delta 6 c
        | c `elem` ['0'..'9']   = Just 6

    delta 7 c
        | c `elem` ['0'..'9']   = Just 7
        | c == '.'              = Just 6

    delta 9 c
        | c == '-'              = Just 10

    delta 10 c
        | c `elem` ['a'..'z']   = Just 10
        | c == '\n'             = Just 11

    delta 12 c
        | c `elem` " \t\n"      = Just 12

    delta _ _                   = Nothing

a1 = mkDFA qs is q0 f delta
  where
    (qs, is, q0, f, delta) = dfa1
    
a2 = convertDFAtoNFA a1

a4 = mkNFA qs is q0 f delta
  where
    (qs, is, q0, f, delta) = nfa4

a5 = addAttr a4Label a4
  where
    a4Label :: Q -> Set String
    a4Label q
      = maybe empty singleton $
        lookup q $
        [ (10,"WS")
        , (9,"IF")
        , (4,"ID")
        , (6,"ID")
        , (8,"NUM")
        ]

a5a = addStateAttr a5

a6 :: DFA' (Set Q) (Set String, ())
a6 = convertNFAtoDFA a5

a7 :: DFA' (Set Q) (Set Q, (Set String, ()))
a7 = addStateAttr a6

a7a :: DFA' (Set Q) (Set (Set Q), (Set String, ()))
a7a = mapSetAttr a7

a8 :: DFA' Q (Set Q, (Set String, ()))
a8 = removeSetsDFA a7

a8a :: DFA' Q (Set (Set Q), (Set String, ()))
a8a = removeSetsDFA a7a

a9 :: DFA' (Set Q) (Set Q, (Set String, ()))
a9 = minDFA a8

t1 = scanDFA'' a1 "if id 1 1.0 .0 --x\n"
t2 = scanNFA'' a2 "if id 1 1.0 .0 --x\n"
t4 = scanNFA'' a4 "if id 1 123"
t5 = scanNFA'' a5 "if id 1 123"

c1 = putStrLn $ genCodeDFA "dfa1" a1
c2 = putStrLn $ genCodeNFA "dnfa1" a2
c4 = putStrLn $ genCodeNFA "nfa4" a4
c5 = putStrLn $ genCodeNFA' "Q"       "(Set String, ())"          "lnfa4"  a5
c6 = putStrLn $ genCodeDFA' "(Set Q)" "(Set String, ())"          "ldfa4"  a6
c7 = putStrLn $ genCodeDFA' "(Set Q)" "(Set Q, (Set String, ()))" "sldfa4" a7
c7a= putStrLn $ genCodeDFA' "(Set Q)" "(Set (Set Q), (Set String, ()))" "sldfa4" a7a
c8 = putStrLn $ genCodeDFA' "Q"       "(Set Q, (Set String, ()))" "ldfa4"  a8
c8a= putStrLn $ genCodeDFA' "Q"       "(Set (Set Q), (Set String, ()))" "ldfa4"  a8a
c9 = putStrLn $ genCodeDFA' "(Set Q)" "(Set Q, (Set String, ()))" "ldfa4"  a9

-- ----------------------------------------

-- nfa4    :: NFA
nfa4
    = (states, alphabet, q0, f, delta)
    where
    states      = [1..10]
    alphabet    = " -" ++ ['a'..'z'] ++ ['0'..'9']
    q0          = 1
    f           = [2]

    delta 1 Nothing             = [7]
    delta 1 (Just c)
        | c == 'i'              = [3,4]
        | c `elem` ['a'..'h']
          ||
          c `elem` ['j'..'z']   = [4]
        | c `elem` [' ', '-']   = [10]

    delta 10 Nothing            = [2]
    delta 3 (Just 'f')          = [9]
    delta 9 Nothing             = [2]
    delta 4 Nothing             = [2,5]

    delta 5 (Just c)
        | c `elem` ['0'..'9']
          ||
          c `elem` ['a'..'z']   = [6]
 
    delta 6 Nothing             = [2,5]

    delta 7 (Just c)
        | c `elem` ['0'..'9']   = [8]

    delta 8 Nothing             = [2,7]

    delta _ _                   = []

xxx :: DFA' Q (Q, ())
xxx = A qs is q0 fs delta attr
  where
    qs    = fromList $ [1..5]
    is    = fromList $ ['0'..'9']
                       ++ ['a'..'z']
    q0    = 1
    fs    = fromList $ [2..5]
    
    delta q i = case q of
                  1
                    | i == 'i'
                      -> Just $ 2
                    | i >= 'g' && i <= 'h'
                      || i >= 'j' && i <= 'z'
                      -> Just $ 3
                    | i >= 'a' && i <= 'f'
                      -> Just $ 4
                    | i >= '0' && i <= '9'
                      -> Just $ 5
                  2
                    | i >= 'a' && i <= 'z'
                      -> Just $ 3
                  3
                    | i >= 'a' && i <= 'z'
                      -> Just $ 3
                  4
                    | i >= 'g' && i <= 'z'
                      -> Just $ 3
                    | i >= 'a' && i <= 'f'
                      -> Just $ 4
                    | i >= '0' && i <= '9'
                      -> Just $ 5
                  5
                    | i >= '0' && i <= '9'
                      || i >= 'a' && i <= 'f'
                      -> Just $ 5
                  _ -> Nothing
    
    attr q = case q of
               1
                 -> (1,())
               2
                 -> (2,())
               3
                 -> (3,())
               4
                 -> (4,())
               5
                 -> (5,())
               _ -> error "genCodeAT: illegal arg"


spec1
  = [ ("IF", "if")
    , ("ID", "[a-z][a-z0-9]*")
    , ("NUM", "[0-9]+")
    , ("REAL", "([0-9]+[.][0-9]*)|([.][0-9]+)")
{-
    , ("WS", "[ \n\t]+")
    , ("CMT", "--[a-z]*\n")
    , ("ERR", ".")
-- -}      
    ]
    
c11 = putStrLn $ genDotNFA "xxx" $ either error id $ scanSpecToNFA spec1
