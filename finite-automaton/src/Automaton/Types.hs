module Automaton.Types where

import           Data.Set.Simple ( Set, fromList )

-- ----------------------------------------

type I = Char

type Q = Int

type Input = [I]

type Token = [I]

type Tokens q = ([(q, Token)], Input)

data Automaton delta q a =
  A { _states         :: Set q
    , _alphabet       :: Set I
    , _startState     :: q
    , _finalStates    :: Set q
    , _delta          :: delta
    , _attr           :: q -> a
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
  = A { _states         = qs'
      , _alphabet       = is'
      , _startState     = q0
      , _finalStates    = fs'
      , _delta          = delta'
      , _attr           = const ()
      }
  where
    is'             = fromList is
    qs'             = fromList qs
    fs'             = fromList fs

mkNFA :: [Q] -> [I] -> Q -> [Q] -> (Q -> Maybe I -> Set Q) -> NFA
mkNFA qs is q0 fs delta'
  = A { _states         = qs'
      , _alphabet       = is'
      , _startState     = q0
      , _finalStates    = fs'
      , _delta          = delta'
      , _attr           = const ()
      }
  where
    is'             = fromList is
    qs'             = fromList qs
    fs'             = fromList fs

-- ----------------------------------------
