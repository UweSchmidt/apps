module Examples
       ( nfaExamples
       , scanExamples
       , reExamples
       )
where

import Automaton.Types
import Automaton.Transform
import Automaton.ScanSpec

import Control.Arrow ((***))

import Data.Set.Simple
import Data.List (intercalate)

-- ----------------------------------------

scanExamples :: [(String, [(String, String)])]
scanExamples
  = [ ("scan1", scan1)
    , ("scan2", scan2)
    , ("scan3", scan3)
    ]

scan1 :: [(String, String)]
scan1
  = [ ("IF", "if")
    , ("ID", "[a-z][a-z0-9]*")
    ]

scan2 :: [(String, String)]
scan2
  = [ ("IF", "if")
    , ("ID", "[a-z][a-z0-9]*")
    , ("NUM", "[0-9]+")
    , ("REAL", "([0-9]+[.][0-9]*)|([.][0-9]+)")
    ]

scan3 :: [(String, String)]
scan3
  = [ ("IF", "if")
    , ("ID", "[a-z][a-z0-9]*")
    , ("NUM", "[0-9]+")
    , ("REAL", "([0-9]+[.][0-9]*)|([.][0-9]+)")
    , ("WS", "[ \n\t]+")
    , ("CMT", "--[a-z]*\n")
    , ("ERR", ".")
    ]

scanToRe :: [(String, String)] -> String
scanToRe
  = intercalate "|" . map (par . snd)
  where
    par xs = "(" ++ xs ++ ")"

reExamples :: [(String, String)]
reExamples
  = map (rename *** scanToRe) scanExamples
  where
    rename = ("regex" ++) . drop 4

-- ----------------------------------------

nfaExamples :: [(String, NFA' Q (Set Q, (PrioLabel String, ())))]
nfaExamples
  = [ ("auto1", dfa1)
    , ("auto2", nfa2)
    ]
    
dfa1 :: NFA' Q (Set Q, (PrioLabel String, ()))
dfa1
  = addStateSetAttr . addAttr (const mkPrio0) $
    convertDFAtoNFA $
    mkDFA states alphabet q0 fs delta
  where
    states      = [1..7] ++ [9..13]
    alphabet    = "\n\t" ++ [' '..'~']
    q0          = 1
    fs          = [2,3,4,5,6,7,9,11,12,13]

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


nfa2 :: NFA' Q (Set Q, (PrioLabel String, ()))
nfa2
  = addStateSetAttr . addAttr labels $
    mkNFA states alphabet q0 fs delta
  where
    states      = [1..10]
    alphabet    = " -" ++ ['a'..'z'] ++ ['0'..'9']
    q0          = 1
    fs          = [2]

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

    labels q
      = maybe mkPrio0 id $
        lookup q $
        [ (10, mkPrio "WS" 10)
        , (9,  mkPrio "IF" 1)
        , (4,  mkPrio "ID" 2)
        , (6,  mkPrio "ID" 2)
        , (8,  mkPrio "NUM" 3)
        ]

-- ----------------------------------------
