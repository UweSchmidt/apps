module Examples
       ( nfaExamples
       , scanExamples
       , reExamples
       )
where

import Automaton.Types
import Automaton.Transform
import Automaton.ScanSpec

import Regex.Core
import Regex.Parse ( parseRegex )

import Data.Set.Simple
-- import Data.List (intercalate)

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

{-
scanToRe :: [(String, String)] -> String
scanToRe
  = intercalate "|" . map (par . snd)
  where
    par xs = "(" ++ xs ++ ")"
-- -}

reExamples :: [(String, Regex)]
reExamples
  = zipWith (\ i r -> ("re" ++ show i, r)) [(1::Int)..] $
    [ REnull
    , REepsilon
    , a
    , REsymset $ fromList "abc"
    , REsymseq "abc"
    , RErep a
    , RErep1 a
    , REopt a
    , REalt aa' aaa'                          -- (aa)*|(aaa)*

    , REalt REepsilon (REalt aa'' aaa'')      -- |(aa)+|(aaa)+

    , foldr1 REseq [ RErep c                  -- c*a(a|c)*b(a|b|c)*
                   , a
                   , RErep (REalt a c)
                   , b
                   , RErep (REalt a (REalt b c))
                   ]
                                                -- ((b|c)*a(b|c)*a)*(b|c)*
    , REseq (RErep (foldr1 REseq [ bc', a, bc', a])) bc'

                                                -- if|<identifier>
    , REalt (REsymseq "if") ident

                                                -- if|then|else|<identifier>
    , foldr1 REalt (map REsymseq [ "if"
                                   , "then"
                                   , "else"] ++ [ident])

    , foldr1 REalt (map REsymseq [ "if"
                                 , "then"
                                 , "else"
                                 , "while"
                                 , "do"
                                 , "for"
                                 , "to"
                                 , "begin"
                                 , "end"
                                 ] ++ [ident, num])
    ]
    ++
    exs
  where
    a           = REsymbol 'a'
    b           = REsymbol 'b'
    c           = REsymbol 'c'
    bc          = REsymset $ fromList "bc"
    bc'         = RErep bc
    aa'         = RErep (REsymseq "aa")         -- (aa)*
    aaa'        = RErep (REsymseq "aaa")        -- (aaa)*
    aa''        = RErep1 (REsymseq "aa")        -- (aa)+
    aaa''       = RErep1 (REsymseq "aaa")       -- (aaa)+
    ident       = REseq
                  (REsymset $ fromList ['a'..'z'])
                  (RErep (REsymset $ fromList (['a'..'z'] ++ ['0'..'9'])))
    num         = RErep1 (REsymset $ fromList "0123456789")

exs     :: [RE]
exs
    = map (either (error "syntax error in builtin regex") id . parseRegex)
      [ "(aa)*|(aaa)*"
      , "|(aa)+|(aaa)+"
      , "if|then|else|[a-z][a-z0-9]*"
      , "|a|bb"
      , "/[*]([^*]|[*]+[^/*])*[*]+/"            -- C comment
      , "/-([/0]|-+0)*-+/"                      -- /-0000-0----0----/ (simple form of C comment)
      , "((b|c)*a(b|c)*a)*(b|c)*"
      ]

-- ----------------------------------------

type NFA'' = NFA' Q (Set Q, (PrioLabel String, ()))
  
nfaExamples :: [(String, NFA'')]
nfaExamples
  = zipWith (\ i a -> ("nfa" ++ show i, a)) [(1::Int)..]
    [ nfa1a
    , nfa1b
    , nfa3
    , nfa4
    , nfa2
    ]
    ++
    zipWith (\ i a -> ("dfa" ++ show i, a)) [(1::Int)..]
    [ dfa1
    , dfa2
    , dfa10
    ]

mkNFA' :: [Q] -> [I] -> Q -> [Q] -> (Q -> Maybe I -> [Q]) -> NFA''
mkNFA' states alphabet q0 fs delta
  = addStateSetAttr . addAttr (const mkPrio0) $
    mkNFA states alphabet q0 fs delta

mkDFA' :: [Q] -> [I] -> Q -> [Q] -> (Q -> I -> Maybe Q) -> NFA''
mkDFA' states alphabet q0 fs delta
  = addStateSetAttr . addAttr (const mkPrio0) $
    convertDFAtoNFA $
    mkDFA states alphabet q0 fs delta

-- ----------------------------------------

nfa1a :: NFA''
nfa1a
  = mkNFA' states alphabet q0 f delta
  where
    states      = [1..6]
    alphabet    = "a"
    q0          = 1
    f           = [1,4,6]
    delta 1 (Just 'a') = [2,5]
    delta 2 (Just 'a') = [3]
    delta 3 (Just 'a') = [4]
    delta 4 (Just 'a') = [2]
    delta 5 (Just 'a') = [6]
    delta 6 (Just 'a') = [5]
    delta _ _          = []

nfa1b :: NFA''
nfa1b
  = mkNFA' states alphabet q0 f delta
  where
    states      = [1..6]
    alphabet    = "a"
    q0          = 1
    f           = [2,5]
    delta 1 Nothing    = [2,5]
    delta 2 (Just 'a') = [3]
    delta 3 (Just 'a') = [4]
    delta 4 (Just 'a') = [2]
    delta 5 (Just 'a') = [6]
    delta 6 (Just 'a') = [5]
    delta _ _          = []

-- --------------------

nfa3 :: NFA''
nfa3
  = mkNFA' states alphabet q0 f delta
  where
    states      = [1..13]
    alphabet    = "\n\t" ++ [' '..'~']
    q0          = 1
    f           = [3,4,5,7,9,12,13]
    delta 1 (Just c)
        | c == 'i'              = [2,4,13]
        | c `elem` ['a'..'z']   = [4,13]
        | c `elem` ['0'..'9']   = [5,6,13]
        | c == '.'              = [8,13]
        | c == '-'              = [10,13]
        | otherwise             = [13]

    delta 2 (Just 'f')          = [3]

    delta 4 (Just c)
        | c `elem` ['a'..'z']
          ||
          c `elem` ['0'..'9']   = [4]

    delta 5 (Just c)
        | c `elem` ['0'..'9']   = [5]

    delta 6 (Just c)
        | c `elem` ['0'..'9']   = [6]
        | c == '.'              = [7]

    delta 7 (Just c)
        | c `elem` ['0'..'9']   = [7]

    delta 8 (Just c)
        | c `elem` ['0'..'9']   = [9]

    delta 9 (Just c)
        | c `elem` ['0'..'9']   = [9]

    delta 10 (Just '-')         = [11]

    delta 11 (Just c)
        | c `elem` ['a'..'z']   = [11]
        | c == '\n'             = [12]

    delta _ _                   = []

nfa4 :: NFA''
nfa4
  = mkNFA' states alphabet q0 f delta
  where
    states      = [1..8]
    alphabet    = " -" ++ ['a'..'z'] ++ ['0'..'9']
    q0          = 1
    f           = [2]

    delta 1 Nothing             = [7]
    delta 1 (Just c)
        | c == 'i'              = [3,4]
        | c `elem` ['a'..'h']
          ||
          c `elem` ['j'..'z']   = [4]
        | c `elem` [' ', '-']   = [2]

    delta 3 (Just 'f')          = [2]

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

nfa2 :: NFA''
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

-- --------------------

dfa1 :: NFA''
dfa1
  = mkDFA' states alphabet q0 f delta
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

dfa2 :: NFA''
dfa2
  = mkDFA' states alphabet q0 f delta
  where
    states      = [1..4]
    alphabet    = "ab"
    q0          = 1
    f           = [2,4]
    delta 1 'a' = Just 2
    delta 1 'b' = Just 4
    delta 2 'a' = Just 3
    delta 3 'a' = Just 3
    delta 3 'b' = Just 4
    delta _ _   = Nothing

-- --------------------
    
dfa10 :: NFA''
dfa10
  = mkDFA' states alphabet q0 fs delta
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

-- ----------------------------------------

