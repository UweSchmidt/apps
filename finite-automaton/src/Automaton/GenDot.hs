{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}

module Automaton.GenDot where

import Automaton.Types (DFA', NFA', I, Q, Automaton(A))

import Data.List       (intercalate)
import Data.Set.Simple
import Data.Map.Simple

import Text.Utils

-- import Debug.Trace(traceShow)

-- ----------------------------------------

type DotFlags = ((Int, Int), String)

-- ----------------------------------------

genDotDFA :: (GenDotAttr a) =>
             DotFlags ->
             String ->
             DFA' Q a -> String
genDotDFA flags name 
  = unlines .
    genDotAutomaton flags genDotDeltaDFA name

genDotDeltaDFA :: Set Q -> Set I -> (Q -> I -> Maybe Q) -> Prog
genDotDeltaDFA qs is delta
  = genDotDelta (foldMap (\ q -> [(q, delta1 q)]) qs)
  where
    delta1 q
      = toListMap $ foldr (\ (i, q') m -> insertMap q' (singleton i) m) emptyMap ips
      where
        ips = foldMap (\ i -> case delta q i of
                        Nothing -> []
                        Just q' -> [(i, q')]
                      ) is

genDotNFA :: (GenDotAttr a) =>
             DotFlags ->
             String ->
             NFA' Q a -> String
genDotNFA flags name 
  = unlines .
    genDotAutomaton flags genDotDeltaNFA name

genDotDeltaNFA :: Set Q -> Set I -> (Q -> Maybe I -> Set Q) -> Prog
genDotDeltaNFA qs is delta
  = genDotDelta (foldMap (\ q -> [(q, delta1 q)]) qs)
    ++ genDEps deltaEps
  where
    delta1 q
      = toListMap $
        foldr (\ (i, qs') m ->
                foldr (\ q' m' -> insertMap q' (singleton i) m') m qs'
              ) emptyMap ips
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
                          
    genDEps
      = concatMap genCase'
      where
        genCase' (q', qs')
          = foldMap genCase'' qs'
          where
            genCase'' q1'
              = pr (show q' ++ " -> " ++ show q1'
                    ++ " [label=\"e\", fontname=Symbol, fontcolor=red];")

genDotAutomaton :: (GenDotAttr a) =>
                   DotFlags ->
                   (Set Q -> Set I -> delta -> Prog) ->
                   String ->
                   Automaton delta Q a -> Prog
genDotAutomaton ((fontSizeG, fontSizeE), cssRef) genEdges name (A qs is q0 fs delta attr)
  = ( pr ("digraph " ++ name ++ " {")
      +> [ "rankdir=LR;"
         , "fondname=" ++ fontName ++ ";"
         , "fontsize=" ++ show fontSizeG ++ ";"
         , "nodesep=.2;"
         , "stylesheet=\"" ++ cssRef ++ "\";"
         ]
      ++ nl
      ++ ( "node " ++> [ "[fontname=" ++ fontName
                       , ",fontsize=" ++ show fontSizeG
                       , ",fixedsize=false"
                       , ",style=filled"
                       , ",color=" ++ color
                       , ",fillcolor=" ++ fillColor
                       , "];"
                       ]
         )
      ++ nl
      ++ ( "edge " ++> [ "[fontname=" ++ fontName
                       , ",fontsize=" ++ show fontSizeE
                       , ",color=" ++ color
                       , "];"
                       ]
         )
      ++ nl
      ++ genNodes qs
      ++ nl
      ++ genEdges qs is delta
      ++ nl
      ++ ( "Start " ++> [ "[width=.1"
                        , ",shape=plaintext"
                        , ",label=\"\""
                        , ",fillcolor=white"
                        , ",color=white"
                        , "];"
                        ]
         )
      ++ pr ("Start -> " ++ show q0 ++ ";")
    )
    ++ pr "}"
  where
    genNodes qs'
      = foldMap genNode qs'
      where
        genNode q
          = pr (show q ++ " [shape=circle" ++ genLabel ++ genCircle ++ "];")
          where
            genCircle
              | q `member` fs
                  = ", peripheries=2"
              | otherwise
                  = ""
            genLabel
              = ", label=\"" ++ genDotAttr (attr q) ++ "\""
        
    fontName  = "Courier"
    color     = "steelblue"
    fillColor = "lightgrey"

genDotDelta ::[(Q, [(Q, Set I)])] -> Prog
genDotDelta qmap
      = concatMap genDot1 qmap
      where
        genDot1 (q, qis)
          = concatMap genDot11 qis
          where
            genDot11 (q1, is')
              = pr (show q ++ " -> " ++
                    show q1 ++ " [label=\"" ++
                    genDotInterval (toList is') ++ "\"];"
                   )

-- --------------------

class GenDotAttr a where
  genDotAttr  :: a -> String
  genDotAttr' :: a -> String
  genDotAttr' = genDotAttr
  
instance GenDotAttr () where
  genDotAttr = const ""

instance GenDotAttr Q where
  genDotAttr = show

instance GenDotAttr q => GenDotAttr (Set q) where
  genDotAttr = intercalate "," . foldMap (\ q -> [genDotAttr q])

instance (GenDotAttr a, GenDotAttr b) => GenDotAttr (a, b) where
  genDotAttr  (x1, x2) = concDotAttr "\\n" (genDotAttr  x1) (genDotAttr  x2)
  genDotAttr' (x1, x2) = concDotAttr " "   (genDotAttr' x1) (genDotAttr' x2)
  
instance GenDotAttr String where
  genDotAttr = id

concDotAttr :: String -> String -> String -> String
concDotAttr _  "" s2 = s2
concDotAttr _  s1 "" = s1
concDotAttr d  s1 s2 = s1 ++ d ++ s2

-- --------------------

genDotInterval    :: [I] -> String
genDotInterval
    = quoteInterval . tail . init . show . formatInterval . interval
      where
      quoteInterval cs
          = concatMap quoteC cs
            where
            quoteC '\"' = "\\\""
            quoteC '\\' = "\\\\"
            quoteC c    = [c]
      formatInterval cs
          | cs == "."
              = "[.]"
          | length cs <= 1
              = cs
          | otherwise
              = "[" ++ cs ++ "]"
      interval (c1:c2:cs)
          | fromEnum c2 == fromEnum c1 + 1
              = c1 : '-' : interval' (c2:cs)
          | otherwise
              = c1 : interval (c2:cs)
      interval cs = cs

      interval' (c1:c2:cs)
          | fromEnum c2 == fromEnum c1 + 1
              = interval' (c2:cs)
      interval' (c1:cs)
          = c1 : interval cs
      interval' _cs
          = error "interval': illegal argument"
            
-- ----------------------------------------
