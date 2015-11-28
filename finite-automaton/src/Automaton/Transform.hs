{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Automaton.Transform where

import Automaton.Types
import Automaton.Run (deltaOnSets,epsilonClosure)

import Data.Set.Simple
import Data.Map.Simple
import Data.Maybe

-- import Debug.Trace (traceShow)

-- ----------------------------------------
--
-- operations on DFA'a

-- union of two NFA's

unionNFA :: Monoid a => NFA' Q a -> NFA' Q a -> NFA' Q a
unionNFA (A qs1 is1 q01 fs1 delta1 attr1) a2'@(A {_states = qs2'})
  = A qs  is  q0  fs  delta  attr
  where
    A qs2 is2 q02 fs2 delta2 attr2
      = renameAutomaton ((+ shift2), (+(-shift2))) a2'
      where
        shift2
          = findMax qs1 + 1 - findMin qs2'

    qs = qs1 `union` qs2 `union` singleton q0
    is = is1 `union` is2
    q0 = maxqs2 + 1
    fs = fs1 `union` fs2

    delta q i
      | q <= maxqs1 = delta1 q i
      | q <= maxqs2 = delta2 q i
    delta q Nothing   -- e transitions to old start states
      | q == q0     = fromList [q01, q02]
    delta _ _       = empty

    attr q
      | q <= maxqs1 = attr1 q
      | q <= maxqs2 = attr2 q
      | otherwise   = mempty  -- new start state
    
    maxqs1 = findMax qs1
    maxqs2 = findMax qs2
    

-- renameNFA :: Eq q => (q -> q1, q1 -> q) -> NFA' q a -> NFA' q1 a

-- we need just a single function for renaming
-- for DFA's i ~ I, for NFA's i ~ Just I
-- for DFA's f ~ Maybe, for NFA's f ~ Set
    
renameAutomaton :: (Eq q1, Functor f) =>
                   (q1 -> q2, q2 -> q1)
                   -> Automaton (q1 -> i -> f q1) q1 a
                   -> Automaton (q2 -> i -> f q2) q2 a
renameAutomaton (f, f1) (A qs is q0 fs delta attr)
  | isBijection
      = A qs' is q0' fs' delta' attr'
  | otherwise
      = error "renameNFA: not a bijection"
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
  = renameAutomaton (f, f1) a
  where
    stateSeq  = zip (toList qs) [1..]
    stateSeq1 = map (\ (x, y) -> (y, x)) stateSeq
    
    f  q  = fromJust $ lookup q  stateSeq
    f1 q1 = fromJust $ lookup q1 stateSeq1

type Partition q = Set (Set q)

minDFA :: (Ord q, Monoid a) => DFA' q a -> DFA' (Set q) a
minDFA = minDFA' (const ())

-- in a scanner the final states must be partitioned
-- with respect to the token detected (ID, IF, ..)
-- so states labeled with the token symbol are'nt equivalent

minDFA' :: (Ord q, Monoid a, Ord a1) =>
           (q -> a1) ->   -- partition function for final states
           DFA' q a -> DFA' (Set q) a
minDFA' pf (A qs is q0 fs delta attr)
  = A qs' is q0' fs' delta' attr'
  where
    prt = insert (qs `difference` fs) $ partitionBy pf fs
    qs' = minPart prt
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
  = renameAutomaton (f, f1) a
  where
    f    = findMin
    f1 q = lookupSet q qs

-- ----------------------------------------
--
-- operations on NFA's

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

addStateSetAttr :: Automaton delta q a -> Automaton delta q (Set q, a)
addStateSetAttr = mapSetAttr . addStateAttr

mapAttr :: (a -> a1) -> Automaton delta q a -> Automaton delta q a1
mapAttr f (A qs is q0 fs delta attr)
  = A qs is q0 fs delta attr'
    where
      attr' = fmap f attr

mapSetAttr :: Automaton delta q (a, b) -> Automaton delta q (Set a, b)
mapSetAttr = mapAttr (\ (x ,y) -> (singleton x, y))

-- ----------------------------------------
