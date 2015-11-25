{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}

module Automaton.Core where

import Automaton.Types

import Data.Set.Simple
import Data.Map.Simple
import Data.Maybe

-- import Debug.Trace (traceShow)

-- ----------------------------------------
--
-- operations on DFA'a

-- word test
    
acceptDFA :: Ord q => DFA' q a -> Input -> Bool
acceptDFA dfa input
  = case scan1DFA dfa input of
     ([(_q, _t)], "") -> True
     _                -> False

-- break an input into tokens
     
scanDFA :: Ord q => DFA' q a -> Input -> Either Input [(q, Token)]
scanDFA dfa input0
  = cont input0
  where
    cont input
      = case scan1DFA dfa input of
         ([], is) -> Left is
         (t1, "") -> Right t1
         (t1, is) -> case cont is of
                      Left rest -> Left rest
                      Right ts  -> Right (t1 ++ ts)


scanDFA' :: Ord q => DFA' q a -> Input -> ([(q, Token)], Input)
scanDFA' dfa input0
  = cont input0
  where
    cont input
      = case scan1DFA dfa input of
         ([], is) -> ([], is)
         (t1, "") -> (t1, "")
         (t1, is) -> let (res, rest) = cont is in
                      (t1 ++ res, rest)

scanDFA'' :: Ord q => DFA' q a -> Input -> ([(a, Token)], Input)
scanDFA'' dfa@(A { _attr = f }) input
  = ( map (\ (q, t) -> (f q, t)) qs, is)
  where
    (qs, is) = scanDFA' dfa input
    
-- split a single token from an input
                      
scan1DFA :: Ord q => DFA' q a -> Input -> Tokens q
scan1DFA (A { _startState  = q0
            , _delta       = delta'
            , _finalStates = fs
            }) is
  
  | null is && q0 `member` fs
     = ([(q0, is)], "")
       
  | otherwise
     = case symbol Nothing (q0, "", is) of
        Nothing          -> ([], is)
        Just (q, t, is') -> ([(q, t)], is')

  where
    symbol lastFinalState curState@(q, t, i)
      |     finalState &&     longestMatch = Just curState
      |     finalState && not longestMatch = symbol (Just curState) nextState
      | not finalState &&     longestMatch = lastFinalState
      | not finalState && not longestMatch = symbol lastFinalState  nextState
      | otherwise                          = error "the impossible happend"

      where
        finalState   = q `member` fs
        longestMatch = null i
                       ||
                       isNothing (delta' q nextChar)
        nextChar     = head i
        nextState    = ( fromJust (delta' q nextChar)
                       , t ++ [nextChar]
                       , tail i
                       )

-- rename the state set by a given bijection
        
renameDFA :: Eq q => (q -> q1, q1 -> q) -> DFA' q a -> DFA' q1 a
renameDFA (f, f1) (A qs is q0 fs delta attr)
  | isBijection
      = A qs' is q0' fs' delta' attr'
  | otherwise
      = error "renameDFA: not a bijection"
    where
      qs'   = fmap f qs
      q0'   =      f q0
      fs'   = fmap f fs
      delta' q1 i
            = fmap f $ delta (f1 q1) i
      attr' = attr . f1

      isBijection = (fmap f1 . fmap f $ qs) == qs

-- (trivial) conversion of a DFA into a NFA
      
convertDFAtoNFA :: DFA' q a -> NFA' q a
convertDFAtoNFA (A qs is q0 fs delta attr)
  = A { _states         = qs
      , _alphabet       = is
      , _startState     = q0
      , _finalStates    = fs
      , _delta          = delta'
      , _attr           = attr
      }
  where
    delta' _  Nothing  = empty
    delta' q' (Just i) = maybe empty singleton $
                         delta q' i

removeSetsDFA :: (Eq q) => DFA' (Set q) a -> DFA' Q a
removeSetsDFA a@(A { _states = qs })
  = renameDFA (f, f1) a
  where
    stateSeq  = zip (toList qs) [1..]
    stateSeq1 = map (\ (x, y) -> (y, x)) stateSeq
    
    f  q  = fromJust $ lookup q  stateSeq
    f1 q1 = fromJust $ lookup q1 stateSeq1

type Partition q = Set (Set q)

minDFA :: (Ord q, Monoid a) => DFA' q a -> DFA' (Set q) a
minDFA (A qs is q0 fs delta attr)
  = A qs' is q0' fs' delta' attr'
  where
    qs' = minPart (fromList [fs, qs `difference` fs])
    q0' = lookupSet q0 qs'
    fs' = filterSet (`isSubsetOf` fs) qs'

    delta' q' i'
      | isEmpty q1' = Nothing
      | otherwise   = Just q1'
      where
        q1' = toPart $ fromList $ catMaybes $ foldMap (\ x -> [delta x i']) q' 
        toPart p
          | isEmpty p = empty
          | otherwise = lookupSubset p qs'

    attr' q'
      = foldMap attr q'

    minPart part
      | part == part' = part
      | otherwise     = minPart part'
      where
        part' = deltaPartS is delta part
        
deltaPartS :: (Ord q) => Set I -> (q -> I -> Maybe q) -> Partition q -> Partition q
deltaPartS is delta part
  = foldr (\ i p -> deltaPart i delta p) part is
    
deltaPart :: (Ord q) => I -> (q -> I -> Maybe q) -> Partition q -> Partition q
deltaPart i delta part
  = foldMap (\ qs -> deltaPart1 qs i delta part) part
    
deltaPart1 :: (Ord q) => Set q -> I -> (q -> I -> Maybe q) -> Partition q -> Set (Set q)
deltaPart1 qs i delta part
  = fromList . elems $ deltaP
  where
    deltaP = foldMap delta' qs
      where
        delta' q'
          = case delta q' i of
             Nothing  -> singletonMap empty        (singleton q')
             Just q1' -> singletonMap (partOf q1') (singleton q')
          where
            partOf q1'
              = q1' `lookupSet` part
                
      
  -- qs is a set of pairwise disjoint sets of numbers
  -- the smallest number in every set is taken to represent
  -- the set

removeSetsDFAMin :: Ord q => DFA' (Set q) a -> DFA' q a
removeSetsDFAMin a@(A { _states = qs })
  = renameDFA (f, f1) a
  where
    f    = findMin
    f1 q = lookupSet q qs

-- ----------------------------------------
--
-- operations on NFA's

acceptNFA :: Ord q => NFA' q a -> Input -> Bool
acceptNFA dfa input
  = case scan1NFA dfa input of
     ([(_q, _t)], "") -> True
     _                -> False

scanNFA :: Ord q => NFA' q a -> Input -> Either Input [(Set q, Token)]
scanNFA nfa input0
  = cont input0
  where
    cont input
      = case scan1NFA nfa input of
         ([], is) -> Left is
         (t1, "") -> Right t1
         (t1, is) -> case cont is of
                      Left rest -> Left rest
                      Right ts  -> Right (t1 ++ ts)

scanNFA' :: (Ord q) => NFA' q a -> Input -> ([(Set q, Token)], Input)
scanNFA' nfa input0
  = cont input0
  where
    cont input
      = case scan1NFA nfa input of
         ([], is) -> ([], is)
         (t1, "") -> (t1, "")
         (t1, is) -> let (res, rest) = cont is in
                      (t1 ++ res, rest)

scanNFA'' :: (Ord q, Monoid a) => NFA' q a -> Input -> ([(a, Token)], Input)
scanNFA'' nfa@(A { _attr = f }) input
  = ( map (\ (qs, t) -> (foldMap f qs, t)) ts, is)
  where
    (ts, is) = scanNFA' nfa input

scan1NFA :: Ord q => NFA' q a -> Input -> Tokens (Set q)
scan1NFA (A { _startState  = q0
            , _delta       = delta'
            , _finalStates = fs
            }) is
  
  | null is && not (qs0 `disjoint` fs)
     = ([(qs0, is)], "")
       
  | otherwise
     = case symbol Nothing (qs0, "", is) of
        Nothing           -> ([], is)
        Just (qs, t, is') -> ([(qs, t)], is')

  where
    deltaS  = deltaOnSets delta'
    qs0     = epsilonClosure delta' $ singleton q0
    
    symbol lastFinalState curState@(qs, t, i)
      |     finalState &&     longestMatch = Just curState
      |     finalState && not longestMatch = symbol (Just curState) nextState
      | not finalState &&     longestMatch = lastFinalState
      | not finalState && not longestMatch = symbol lastFinalState  nextState
      | otherwise                          = error "the impossible happend"

      where
        finalState   = not (qs `disjoint` fs)
        longestMatch = null i
                       ||
                       isEmpty qs'
        nextChar     = head i
        qs'          = deltaS qs nextChar
        nextState    = ( qs'
                       , t ++ [nextChar]
                       , tail i
                       )

-- epsilon closure of a set of states
        
epsilonClosure  :: Ord q => (q -> Maybe I -> Set q) -> Set q -> Set q
epsilonClosure delta qs
    | card qs == card qs'
        = qs'
    | otherwise
        = epsilonClosure delta qs'
    where
      qs' = foldr union qs $ fmap (flip delta epsilon) qs
      epsilon = Nothing

-- convert delta to work with sets of states as arguments
      
extendDelta :: Ord q => (q -> Maybe I -> Set q) -> (Set q -> I -> Set q)
extendDelta delta
  = delta'
    where
      delta' qs i
        = foldr union empty $ fmap (\ q' -> delta q' (Just i)) qs

deltaOnSets :: Ord q => (q -> Maybe I -> Set q) -> (Set q -> I -> Set q)
deltaOnSets delta
  = \ qs i -> epsilonClosure delta $ (extendDelta delta) qs i

qsToAs :: Monoid a => (q -> a) -> Set q -> a
qsToAs f qs
  = foldMap f qs

scanResToAs :: Monoid a => (q -> a) -> [(Set q, Token)] -> [(a, Token)]
scanResToAs f xs = map (\ (qs, t) -> (qsToAs f qs, t)) xs

-- conversion of a DFA into a NFA
      
convertNFAtoDFA :: (Ord q, Ord (Set q), Monoid a) => NFA' q a -> DFA' (Set q) a
convertNFAtoDFA (A _qs is q0 fs deltaN attr)
  = A { _states         = qs'
      , _alphabet       = is
      , _startState     = q0'
      , _finalStates    = fs'
      , _delta          = delta'
      , _attr           = attr'
      }
  where
    deltaS = deltaOnSets deltaN
    q0'    = epsilonClosure deltaN $ singleton q0
    qs'    = keys deltaMap
    fs'    = filterSet (\ q' -> not (q' `disjoint` fs)) qs'
    attr'  = foldMap attr

    delta' q' i
      | null q1s  = Nothing
      | otherwise = Just (head q1s)
      where
        q'map = toListMap . fromJust $ lookupMap q' deltaMap
        q1s   = map fst . filter (\ p -> i `elem` snd p) $ q'map
           
    deltaMap
      = genDelta' is q0' deltaS

genDelta' :: (Ord q) =>
             Set I -> Set q -> (Set q -> I -> Set q) ->
             Map (Set q) (Map (Set q) (Set I))
genDelta' is q0 delta
  = states (singleton q0) emptyMap
  where
    states open acc
      | isEmpty open
          = acc
      | otherwise
          = states open' (acc `unionMap` acc1)
      where
        acc1   = foldMap stateMap open
        closed = keys acc1
        
        open' = (mconcat . map keys $ elems acc1) `difference` closed
        
        stateMap q = singletonMap q nextMap
          where
            nextMap = foldMap isMap is
              where
                isMap i
                  | isEmpty q1 = emptyMap
                  | otherwise  = singletonMap q1 (singleton i)
                  where
                    q1 = delta q i
        
-- ----------------------------------------

addAttr :: (q -> a1) -> Automaton delta q a -> Automaton delta q (a1, a)
addAttr f (A qs is q0 fs delta attr)
  = A qs is q0 fs delta attr'
    where
      attr' q' = (f q', attr q')

addStateAttr :: Automaton delta q a -> Automaton delta q (q, a)
addStateAttr = addAttr id

mapAttr :: (a -> a1) -> Automaton delta q a -> Automaton delta q a1
mapAttr f (A qs is q0 fs delta attr)
  = A qs is q0 fs delta attr'
    where
      attr' = fmap f attr

mapSetAttr :: Automaton delta q (a, b) -> Automaton delta q (Set a, b)
mapSetAttr = mapAttr (\ (x ,y) -> (singleton x, y))

-- ----------------------------------------
