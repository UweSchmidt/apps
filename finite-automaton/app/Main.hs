module Main where

import Automaton.Types ( Q, NFA', DFA')
import Automaton.Core ( convertNFAtoDFA
                      , removeSetsDFA
                      , removeSetsDFAMin
                      , addStateAttr
                      , mapAttr
                      , mapSetAttr
                      )
import Automaton.GenDot ( GenDotAttr(..)
                        , genDotNFA
                        , genDotDFA
                        )

import Automaton.GenCode (GenCodeSet(..)
                         , GenCodePattern(..)
                         , genCodeNFA'
                         , genCodeDFA'
                         )
import Control.Monad.RWSErrorIO
import qualified  Control.Monad.RWSErrorIO as RWS(exec)

import Data.Set.Simple ( Set )

import Regex.Core ( Regex, reToNFA)
import Regex.Parse ( parseRegex )

import System.Console.CmdTheLine
import System.Console.CmdTheLine.Utils
import System.FilePath (replaceExtension)
import System.Exit

-- imports for examples
import Automaton.Types (mkDFA, mkNFA, Automaton(..))
import Automaton.Core ( convertDFAtoNFA
                      , addAttr
                      , minDFA
                        , scanDFA, scanNFA
                          )
import Automaton.GenCode ( genCodeDFA, genCodeNFA )
import Data.Set.Simple (empty, fromList, singleton)

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
      , theRegex         :: String
      , genNFA           :: (Bool, Bool)
      , genDFASet        :: (Bool, Bool)
      , genDFA           :: (Bool, Bool)
      , genDFAMinSet     :: (Bool, Bool)
      , genDFAMin        :: (Bool, Bool)
      }
    deriving (Show)
             
instance Config Env where
    traceOn   = theTraceFlag
    warningOn = theWarningFlag
    errorOn   = theErrorFlag
    stderrOn  = theStdErrFlag

initEnv
  = Env
    { theProgName      = "finite-automaton"
    , theTraceFlag     = False
    , theWarningFlag   = True
    , theErrorFlag     = True
    , theStdErrFlag    = True
    , theName          = ""
    , theRegex         = ""
    , genNFA           = (True, True)
    , genDFASet        = (True, True)
    , genDFA           = (True, True)
    , genDFAMinSet     = (True, True)
    , genDFAMin        = (True, True)
    }
    
-- ----------------------------------------

oAll :: Term (Env -> Env)
oAll
  = oVerbose
    <.> oQuiet
    <.> oName
    <.> oRegex
    
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
  = fmap setName . value . opt ""
    $ (optInfo ["n", "name"])
            { optName = "NAME"
            , optDoc = "Set the name of the automaton."
            }
  where
    setName n e = e { theName = n }

oRegex :: Term (Env -> Env)
oRegex
  = fmap setRegex . value . opt ""
    $ (optInfo ["r", "regex"])
            { optName = "REGEX"
            , optDoc = "the regular expression to be converted into an automaton."
            }
  where
    setRegex r e = e { theRegex = r }

-- ----------------------------------------

type State = ()

initState :: State
initState = ()

-- ----------------------------------------

type Cmd = Action Env State

doIt :: Cmd ()
doIt
    = do n <- asks theName
         trc $ "start processing automaton " ++ show n
         env <- ask
         trc $ "environment: " ++ show env
         inp <- asks theRegex
         pipeline inp
         trc $ "end processing automaton "   ++ show n

pipeline :: String -> Cmd ()
pipeline
  = stringToRegex
    >=> regexToNFA        >=> teeToDot nfaToDot
    >=> nfaToDFAset       >=> teeToDot dfaSetToDot
    >=> dfaSetToDFA       >=> teeToDot dfaToDot
    >=> dfaToDFAMinSet    >=> teeToDot dfaMinSetToDot
    >=> dfaMinSetToDFAMin >=> teeToDot dfaMinToDot
    >=> (return . const ())
    
    
stringToRegex :: String -> Cmd Regex
stringToRegex xs
  = do trc $ "parse regex: " ++ show xs
       either
         abort
         (\ re -> do trc $ "parsed regex " ++ show re
                     return re
         ) $ parseRegex xs

regexToNFA :: Regex -> Cmd (NFA' Q (Q, ()))
regexToNFA re
  = do trc $ "transform regex into NFA"
       return ( addStateAttr
                . reToNFA
                $ re
              )

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

dfaToDFAMinSet :: (Ord a, Monoid a1) => DFA' Q (a, a1) -> Cmd (DFA' Q (Set a, a1))
dfaToDFAMinSet a
  = do trc $ "minimize DFA into min DFA with sets as states"
       return ( removeSetsDFAMin    -- state sets -> numbers
                . minDFA
                . mapSetAttr        -- make attr a monoid with `union`
                $ a
              )

dfaMinSetToDFAMin :: DFA' Q (a, a1) -> Cmd (DFA' Q (Q, a1))
dfaMinSetToDFAMin a
  = do trc $ "rename set of equiv states in min DFA into simple states"
       return ( addStateAttr        -- add state numbers
                . mapAttr snd       -- remove the state set attribute
                $ a
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
  = asks theName >>= c

teeToDot :: (a -> Cmd ()) -> a -> Cmd a
teeToDot cmd a
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
  = do writeCode dotFile s
       dotToPng  dotFile

writeCode :: FilePath -> String -> Cmd ()
writeCode out d
  = do trc $ "write file " ++ show out
       trc $ d
       io $ writeFile out d
       return ()

dotToPng :: FilePath -> Cmd ()
dotToPng dotFile
  = do trc $ unwords ["convert", dotFile, "to", pngFile]
       RWS.exec "dot" ["-Tpng", "-o" ++ pngFile, dotFile]
  where
    pngFile = replaceExtension dotFile "png"

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

a4 = mkNFA qs is q0 f delta'
  where
    (qs, is, q0, f, delta) = nfa4
    delta' q i = fromList $ delta q i

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

t1 = scanDFA a1 "if id 1 1.0 .0 --x\n"
t2 = scanNFA a2 "if id 1 1.0 .0 --x\n"
t4 = scanNFA a4 "if id 1 123"
t5 = scanNFA a5 "if id 1 123"

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

