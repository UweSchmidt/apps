{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Automaton.ScanSpec where

import Automaton.Types
         ( Q, Automaton(..), NFA')
import Automaton.Transform
         ( addStateAttr
         , addAttr
         , mapSetAttr
         , unionNFA
         )
import Automaton.GenDot ( GenDotAttr(..) )

import Data.Set.Simple ( Set, member )
import Data.Either (partitionEithers)

import Regex.Core ( reToNFA)
import Regex.Parse ( parseRegex )

-- ----------------------------------------

-- used for labeling final states with the token type
-- recognized by a final state
-- the labels contain a priority
-- to decide which token is found, when the tokens
-- ambigious, e.g. with keywords and identifiers
--
-- The labels are optionally, therefor the Maybe type
--
-- PrioLabels form a monoid. In case a final state in a DFA
-- represents a set of final states from the corresponding NFA,
-- the attribute of the DFA state is computed by `mappend`-ing
-- the NFA state attributes

newtype PrioLabel a =
  PL { thePrioLabel :: Maybe (a, Int) }
  deriving (Eq, Ord, Show)

instance Monoid (PrioLabel a) where
  mempty = PL Nothing

  PL Nothing `mappend` p2
    = p2

  p1 `mappend` PL Nothing
    = p1

  -- lower numbers denote higher priority
  
  p1@(PL (Just (_, i1))) `mappend` p2@(PL (Just (_, i2)))
    | i1 <= i2
        = p1
    | otherwise
        = p2

instance GenDotAttr (PrioLabel String) where
  genDotAttr (PL Nothing) = ""
  genDotAttr (PL (Just (x, _))) = x
  
mkPrio0 :: PrioLabel a
mkPrio0 = PL Nothing

mkPrio :: a -> Int -> PrioLabel a
mkPrio x i
  = PL $ Just (x, i)

-- ----------------------------------------

scanSpecToNFA :: [(a, String)] -> Either String (NFA' Q (Set Q, (PrioLabel a, ())))
scanSpecToNFA xs
  | null xs
      = Left "no scanner spec found"
  | null es
      = Right . mapSetAttr . addStateAttr . unionsNFA $ zipWith3 labelNFA ls [1..] as
  | otherwise
      = Left $ head es
  where
    (ls, rs)
      = unzip xs
    (es, as)
      = partitionEithers $
        map (fmap reToNFA . parseRegex) rs

    labelNFA x i a@(A {_finalStates = fs})
      = addAttr attr' $ a
      where
        attr' q
          | q `member` fs = mkPrio x i
          | otherwise     = mkPrio0

    unionsNFA
      = foldl1 unionNFA

-- ----------------------------------------
