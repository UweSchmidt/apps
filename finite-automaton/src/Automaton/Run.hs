module Automaton.Run where

import Automaton.Types

import Data.Set.Simple
import Data.Maybe

-- import Debug.Trace (traceShow)

-- ----------------------------------------
--
-- general operations on FA'a

-- word test

acceptAuto :: (Automaton delta q a -> Input -> ([(q', Token)], Input)) ->
              Automaton delta q a ->
              Input -> Bool
acceptAuto scan1Auto fa input
  = case scan1Auto fa input of
     ([(_q, _t)], "") -> True
     _                -> False

-- and specialisations

acceptDFA :: Ord q => DFA' q a -> Input -> Bool
acceptDFA = acceptAuto scan1DFA

acceptNFA :: Ord q => NFA' q a -> Input -> Bool
acceptNFA = acceptAuto scan1NFA

-- scanner

scanAuto :: (Automaton delta q a -> Input -> ([(q', Token)], Input)) ->
            Automaton delta q a ->
            Input -> ([(q', Token)], Input)

scanAuto scan1Auto fa input0
  | null input0                   -- tricky: if input is empty and empty word is accepted
      = scan1Auto fa input0       -- a single token is produced, else the empty sequence       
  | otherwise
      = cont input0
  where
    cont input
      = case scan1Auto fa input of
         ([], is) -> ([], is)     -- scan got stuck
         
         (((_, "") : t1), is)     -- tricky: token is the empty string
                  -> (t1, is)     -- and q0 `elem` fs: scan also got stuck

         (t1, "") -> (t1, "")     -- scan finished with success
         
         (t1, is) -> let (res, rest) = cont is in
                      (t1 ++ res, rest)

-- and specialisations

scanDFA' :: Ord q => DFA' q a -> Input -> ([(q, Token)], Input)
scanDFA' = scanAuto scan1DFA

scanNFA' :: (Ord q) => NFA' q a -> Input -> ([(Set q, Token)], Input)
scanNFA' = scanAuto scan1NFA

-- convert states to attributes
-- in scanned token sequence

scanAuto' :: ((q -> a) -> (q' -> a)) ->
             (Automaton delta q a -> Input -> ([(q', Token)], Input)) ->
             Automaton delta q a ->
             Input -> ([(a, Token)], Input)
scanAuto' fmp scanFA' fa@(A { _attr = f }) input
  = ( map (\ (q, t) -> (fmp f q, t)) qs, is)
  where
    (qs, is) = scanFA' fa input

scanDFA'' :: Ord q => DFA' q a -> Input -> ([(a, Token)], Input)
scanDFA'' = scanAuto' id scanDFA'

scanNFA'' :: (Ord q, Monoid a) => NFA' q a -> Input -> ([(a, Token)], Input)
scanNFA'' = scanAuto' foldMap scanNFA'

-- ----------------------------------------
--
-- special DFA ops

-- split a single token from an input
                      
scan1DFA :: Ord q =>
            Automaton (q -> I -> Maybe q) q a ->
            Input -> ([(q, Token)], Input)
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

-- --------------------
--
-- special NFA ops

scan1NFA :: Ord q =>
            Automaton (q -> Maybe I -> Set q) q a ->
            Input -> ([(Set q, Token)], Input)
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
      qs' = qs `union` qse
      qse = foldMap (\ q -> delta q epsilon) qs
        where
          epsilon = Nothing
              
-- convert delta to work with sets of states as arguments
      
extendDelta :: Ord q => (q -> Maybe I -> Set q) -> (Set q -> I -> Set q)
extendDelta delta
  = delta'
  where
    delta' qs i
      = foldMap (\ q -> delta q (Just i)) qs

deltaOnSets :: Ord q => (q -> Maybe I -> Set q) -> (Set q -> I -> Set q)
deltaOnSets delta
  = \ qs i -> epsilonClosure delta $ (extendDelta delta) qs i

-- ----------------------------------------
