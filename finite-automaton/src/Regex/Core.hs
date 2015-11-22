module Regex.Core
where

-- TODO: rewrite [I] as Set I
  
import Automaton.Types (I, Q, NFA', mkNFA)

import Data.Set.Simple (mkSet)
import Data.List

type Regex = RE

data RE
  = REnull              -- empty set
  | REepsilon           -- epsilon
  | REsymbol I          -- a for a elem Alphabet
  | REseq RE RE         -- r1 . r2
  | REalt RE RE         -- r1 | r2
  | RErep RE            -- r1 *
  | RErep1 RE           -- r1 +
  | RErepN RE Int               -- r1{n}
  | RErepNstar RE Int           -- r1{n,}
  | RErepN2M RE Int Int -- r1{n,m}
  | REopt RE            -- r1 ?
  | REsymset [I]        -- [abc...]
  | REsymseq [I]        -- "abc"
  deriving (Read, Show)

--

-- estimate the # of states for a given RE
-- used to reject denial of service attacks
-- when generating automata by a web server

statesRE :: RE -> Int
statesRE = (+2) . sizeRE

sizeRE  :: RE -> Int
sizeRE  REnull          = 0                             -- 
sizeRE  REepsilon       = 0                             -- 0 new states
sizeRE (REsymbol _)     = 0                             -- 0 new states
sizeRE (REseq r1 r2)    = 1 + sizeRE r1 + sizeRE r2     -- 1 new state
sizeRE (REalt r1 r2)    =     sizeRE r1 + sizeRE r2     -- 0 new states
sizeRE (RErep r1)       = 2 + sizeRE r1                 -- 2 new states
sizeRE (RErep1 r1)      = 2 + sizeRE r1                 -- 2 new states
sizeRE (RErepN r1 n)    = n * sizeRE r1 + n
sizeRE (RErepNstar r1 n)= n * sizeRE r1 + n + 1
sizeRE (RErepN2M r1 _ m)= m * sizeRE r1 + m
sizeRE (REopt r1)       =     sizeRE r1
sizeRE (REsymset _)     = 0
sizeRE (REsymseq s)     = length s

-- eliminate convienient ops [...], "...", r? and r*

normalizeForNFA :: RE -> RE

normalizeForNFA (REseq r1 r2)   = norm (normalizeForNFA r1) (normalizeForNFA r2)
  where
    norm REnull _               = REnull
    norm _ REnull               = REnull
    norm REepsilon re2          = re2
    norm re1 REepsilon          = re1
    norm re1 re2                = REseq re1 re2

normalizeForNFA (REalt r1 r2)   = norm (normalizeForNFA r1) (normalizeForNFA r2)
  where
    norm REnull re2             = re2
    norm re1 REnull             = re1
    norm re1 re2                = REalt re1 re2

normalizeForNFA (RErep re)      = REalt REepsilon (RErep1 (normalizeForNFA re))

normalizeForNFA (RErep1 re)     = RErep1 (normalizeForNFA re)

normalizeForNFA (RErepN re n)
    | n == 0                    = REepsilon
    | n == 1                    = normalizeForNFA re
    | n >= 2                    = normalizeForNFA (REseq re (RErepN re (n-1)))

normalizeForNFA (RErepNstar re n)
    | n == 0                    = normalizeForNFA (RErep re)
    | n == 1                    = normalizeForNFA (RErep1 re)
    | n >= 2                    = normalizeForNFA (REseq re (RErepNstar re (n-1)))

normalizeForNFA (RErepN2M re n m)
    | n > m                     = REnull
    | n == m                    = normalizeForNFA (RErepN re n)
    | n < m && n == 0           = normalizeForNFA (REseq (REopt re) (RErepN2M re 0 (m-1)))
    | n < m && n > 0            = normalizeForNFA (REseq re (RErepN2M re (n-1) (m-1)))

normalizeForNFA (REopt re)      = REalt REepsilon (normalizeForNFA re)

normalizeForNFA (REsymset ss)   = let
                                  ss' = nub ss
                                  in if null ss'
                                     then REnull
                                     else foldr1 REalt . map REsymbol $ ss'

normalizeForNFA (REsymseq "")   = REepsilon

normalizeForNFA (REsymseq ss)   = foldr1 REseq . map REsymbol $ ss

normalizeForNFA re              = re

-- compute all input symbols used in an R.E.

allSymbols      :: RE -> [I]
allSymbols (REnull)             = []
allSymbols (REepsilon)          = []
allSymbols (REsymbol i)         = [i]
allSymbols (REseq re1 re2)      = allSymbols re1 `union` allSymbols re2
allSymbols (REalt re1 re2)      = allSymbols re1 `union` allSymbols re2
allSymbols (RErep re)           = allSymbols re
allSymbols (RErep1 re)          = allSymbols re
allSymbols (RErepN re _)        = allSymbols re
allSymbols (RErepNstar re _)    = allSymbols re
allSymbols (RErepN2M re _ _)    = allSymbols re
allSymbols (REopt re)           = allSymbols re
allSymbols (REsymset ss)        = nub ss
allSymbols (REsymseq ss)        = nub ss

-- ----------------------------------------

type Delta = Q -> Maybe I -> [Q]

reToNFA :: RE -> NFA' Q ()
reToNFA re
  = mkNFA states syms q0 finalStates delta
  where
    states      = [1..lastState]
    syms        = sort . allSymbols $ re
    q0          = 1
    finalStates = [2]
    delta q i   = mkSet $ delta'' q i
    lastState   = fst delta'
    delta'      = compDelta 1 2 2 (normalizeForNFA re)
    delta''     = snd delta'
    
compDelta       :: Q -> Q -> Q -> RE -> (Q, Q -> Maybe I -> [Q])
compDelta _s _e mx (REnull)
    = (mx, \ _q _i -> [])

compDelta s e mx (REepsilon)
    = (mx, epsilonTrans s e)

compDelta s e mx (REsymbol c)
    = (mx, simpleTrans s c e)

compDelta s e mx (REseq re1 re2)
    = let
      q = mx + 1
      (mx1, delta1) = compDelta s q q   re1
      (mx2, delta2) = compDelta q e mx1 re2
      in
      (mx2, delta1 `orTrans` delta2)

compDelta s e mx (REalt re1 re2)
    = let
      (mx1, delta1) = compDelta s e mx  re1
      (mx2, delta2) = compDelta s e mx1 re2
      in
      (mx2, delta1 `orTrans` delta2)

compDelta s e mx (RErep1 re)
    = let
      q1 = mx + 1
      q2 = mx + 2
      (mx1, delta1) = compDelta q1 q2 q2 re
      in
      (mx1
      , epsilonTrans s  q1 `orTrans`
        epsilonTrans q2 e  `orTrans`
        epsilonTrans q2 q1 `orTrans`
        delta1
      )

compDelta _ _ _ re
    = error ("compDelta undefined for: " ++ show re)

-- ----------------------------------------

epsilonTrans    :: Q -> Q -> Delta
epsilonTrans s e
    = \ q i -> if q == s && i == Nothing  then [e] else []

simpleTrans     :: Q -> I -> Q -> Delta
simpleTrans s c e
    = \ q i -> if q == s && i == (Just c) then [e] else []

orTrans         :: Delta -> Delta -> Delta
delta1 `orTrans` delta2
    = \ q i -> let
               qs1 = delta1 q i
               qs2 = delta2 q i
               in
               qs1 `union` qs2

-- ----------------------------------------
