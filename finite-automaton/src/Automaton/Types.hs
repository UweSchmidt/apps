module Automaton.Types where

import           Data.Set.Simple ( Set, fromList )

-- ----------------------------------------

type I = Char

type Q = Int

type Input = [I]

type Token = [I]

type Tokens q = ([(q, Token)], Input)

data Automaton delta q a =
  A { states         :: Set q
    , alphabet       :: Set I
    , startState     :: q
    , finalStates    :: Set q
    , delta          :: delta
    , attr           :: q -> a
    }

type NFA' q a = Automaton (q -> Maybe I -> Set   q) q a
type DFA' q a = Automaton (q ->       I -> Maybe q) q a

type State'DFA q = (q,     Token, Input)
type State'NFA q = (Set q, Token, Input)

type NFA = NFA' Q ()
type DFA = DFA' Q ()

-- ----------------------------------------
--
-- smart constructors

mkDFA :: [Q] -> [I] -> Q -> [Q] -> (Q -> I -> Maybe Q) -> DFA
mkDFA qs is q0 fs delta'
  = A { states         = qs'
      , alphabet       = is'
      , startState     = q0
      , finalStates    = fs'
      , delta          = delta'
      , attr           = const ()
      }
  where
    is'             = fromList is
    qs'             = fromList qs
    fs'             = fromList fs

mkNFA :: [Q] -> [I] -> Q -> [Q] -> (Q -> Maybe I -> Set Q) -> NFA
mkNFA qs is q0 fs delta'
  = A { states         = qs'
      , alphabet       = is'
      , startState     = q0
      , finalStates    = fs'
      , delta          = delta'
      , attr           = const ()
      }
  where
    is'             = fromList is
    qs'             = fromList qs
    fs'             = fromList fs

-- ----------------------------------------
