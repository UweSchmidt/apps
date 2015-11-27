{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}

module Automaton.GenCode where

import Automaton.Types (DFA, NFA, DFA', NFA', I, Q, Automaton(A))

import Data.Char       ( toUpper )
import Data.Set.Simple
import Data.Map.Simple

import Text.Utils

-- ----------------------------------------

genCodeDFA :: String -> DFA -> String
genCodeDFA = genCodeDFA' "Q" "()"

genCodeDFA' :: (Ord q, Show q, Show a, GenCodePattern q, GenCodeSet q) =>
               String -> String -> String ->
               DFA' q a -> String
genCodeDFA' tq ta name a
  = unlines $
    genCodeAutomaton name sig genCodeDeltaDFA a
  where
    sig = "DFA' " ++ tq ++ " " ++ ta

-- --------------------
    
genCodeNFA :: String -> NFA -> String
genCodeNFA = genCodeNFA' "Q" "()"

genCodeNFA' :: (Ord q, Show q, Show a, GenCodePattern q, GenCodeSet q) =>
               String -> String -> String ->
               NFA' q a -> String
genCodeNFA' tq ta name a
  = unlines $
    genCodeAutomaton name sig genCodeDeltaNFA a
  where
    sig = "NFA' " ++ tq ++ " " ++ ta
    
-- --------------------

genCodeAutomaton :: (Ord q, Show q, Show a, GenCodeSet q, GenCodePattern q) =>
                    String ->
                    String ->
                    (Set q -> Set I -> delta -> Prog) ->
                    Automaton delta q a -> Prog
genCodeAutomaton name sig genCodeDelta (A qs is q0 fs delta attr)
  = []
    ++ pr ("module " ++ mName name ++ " where")
    ++ nl
    ++ pr "import Automaton"
    ++ nl
    ++ pr (name ++ " :: " ++ sig)
    ++ pr (name ++ " = A qs is q0 fs delta attr")
    +> pr "where"
    +> "qs    = " ++> genCodeSet qs
    ++ "is    = " ++> genCodeSet is
    ++ "q0    = " ++> show' q0
    ++ "fs    = " ++> genCodeSet fs
    ++ nl
    ++ genCodeDelta qs is delta
    ++ nl
    ++ genCodeAT    qs attr
--    ++ "attr  = " ++> genCodeAttr qs attr
  where
    mName (x : xs) = toUpper x : xs
    mName _        = "Auto"

genCodeAT :: (Ord q, Show q, Show a, GenCodePattern q) => Set q -> (q -> a) -> Prog
genCodeAT qs attr
  = ("attr q = ")
    ++> ( pr ("case " ++ frPattern dummy "q" ++ " of")
          +> ( concatMap genCases qs
               ++ pr "_ -> error \"genCodeAT: illegal arg\""
             )
        )
  where
    dummy = findMin qs
    genCases q
      = pr (toPattern q)
        +> pr ("-> " ++ show (attr q))
  
genCodeDeltaDFA :: (Ord q, Show q, GenCodePattern q) =>
                   Set q -> Set I -> (q -> I -> Maybe q) -> Prog
genCodeDeltaDFA qs is delta
  = genDelta "i" (pr "_ -> Nothing") (\ s -> "Just $ " ++ s) deltaI
  where
    deltaI
      = foldMap (\ q -> [(q, delta1 q)]) qs

    delta1 q
      = toListMap $ foldr (\ (i, q') m -> insertMap q' [i] m) emptyMap ips
      where
        ips = foldMap (\ i -> case delta q i of
                        Nothing -> []
                        Just q' -> [(i, q')]
                      ) is

genCodeDeltaNFA :: (Ord q, Show q, GenCodePattern q) =>
                   Set q -> Set I -> (q -> Maybe I -> Set q) -> Prog
genCodeDeltaNFA qs is delta
  =    genDelta "(Just i)" (pr "_ -> empty") id deltaI
    ++ genDeltaEps deltaEps
    ++ pr "delta _ _ = empty"
    where
      deltaI
        = foldMap (\ q -> [(q, delta1 q)]) qs
          
      delta1 q
        = toListMap $ foldr (\ (i, q') m -> insertMap q' [i] m) emptyMap ips
          where
            ips = foldMap ( \ i -> case delta q (Just i) of
                                    qs' | isEmpty qs' -> []
                                        | otherwise   -> [(i, qs')]
                          ) is
                  
      deltaEps
        = foldMap (\ q -> eqs q (delta q Nothing)) qs 
        where
          eqs q' qs'
            | isEmpty qs' = []
            | otherwise   = [(q', qs')]

      genDeltaEps = concatMap genCase'
        where
          genCase' (q', qs')
            = ("delta " ++ show q' ++ " Nothing" ++ " = ") ++> show' qs'

genDelta :: (Enum a, Eq a, Show a, GenCodePattern q, Show q, Show q1) =>
            String -> Prog ->
            (String -> String) ->
            [(q, [(q1, [a])])] -> Prog
genDelta jst defCase mkJust qmap
  = ("delta q " ++ jst ++ " = ")
    ++> ( pr ("case " ++ frPattern dummy "q" ++ " of")
          +> ( concatMap genCases qmap
               ++ defCase
             )
        )
  where
    dummy = fst . head $ qmap
    
    genCases (_, []) = []
    genCases (q, qs)
      = pr (toPattern q)
        +> concatMap (genCase mkJust) qs
                  

genCase :: (Enum a, Eq a, Show a, Show q) =>
           (String -> String) -> (q, [a]) -> Prog
genCase mkJust (q', is')
  = "| " ++> ( (gcTest . map toTest . pairsToInterval . toPairs $ is')
               ++ pr ("-> " ++ mkJust (show q'))
             )
  where
    gcTest [] = pr "False"
    gcTest (x : xs) = x ++ concatMap ("|| " ++>) xs

    toTest (Left [c])     = pr ("i == "     ++ show c)
    toTest (Left cs)      = pr ("i `elem` " ++ show cs)
    toTest (Right (l, u)) = pr ("i >= " ++ show l ++ " && i <= " ++ show u)
                        
-- --------------------

class GenCodePattern q where
  toPattern :: q -> String
  frPattern :: q -> String -> String

instance GenCodePattern Q where
  toPattern = show
  frPattern = const id

instance GenCodePattern (Set Q) where
  toPattern = show . toList
  frPattern _ i = "toList " ++ i
  
-- --------------------

class Show a => GenCodeSet a where
  genCodeSet :: Set a -> Prog

instance GenCodeSet I where
  genCodeSet = genCodeSet'

instance GenCodeSet Q where
  genCodeSet = genCodeSet'

instance GenCodeSet q => GenCodeSet (Set q) where
  genCodeSet = genCodeSetSet
  
genCodeSet' :: (Eq a, Enum a, Show a) => Set a -> Prog
genCodeSet' s
  = "fromList $ " ++> (genCodeInterval . pairsToInterval . toPairs . toList $ s)

genCodeSetSet :: (GenCodeSet a) => Set (Set a) -> Prog
genCodeSetSet s
  = "fromList $ " ++> genCode (foldMap ((:[]) . genCodeSet) s)
  where
    genCode :: [Prog] -> Prog
    genCode [] = pr "[]"
    genCode (x : xs) = "[ " ++> x
                       ++ concatMap (", " ++>) xs
                       ++ pr "]"
    
genCodeInterval :: Show a => [Either [a] (a, a)] -> Prog
genCodeInterval [] = pr "[]"
genCodeInterval is
  = p1 ++ concatMap ("++ " ++>) ps
  where
    (p1 : ps) = map toI is
    
    toI (Left  is')    = show' is'
    toI (Right (l, u)) = pr ("[" ++ show l ++ ".." ++ show u ++ "]")

-- ----------------------------------------
