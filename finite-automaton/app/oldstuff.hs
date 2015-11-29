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
