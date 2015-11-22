module Text.Utils where

-- --------------------
-- auxiliary stuff for program formatting
  
type Prog = [String]

infixr 6 ++>
infix  6 +>>
infixr 5 +>

-- indent a prog by prefixing
-- the 1. line with a string

(++>) :: String -> Prog -> Prog
pre ++> []        = [pre]
pre ++> (l1 : ls) = (pre ++ l1) : map (indent ++) ls
  where
    indent = map (const ' ') pre

-- indent a prog by prefixing
-- all lines by a given # of spaces
    
(+>>) :: Int -> Prog -> Prog
_ +>> [] = []
0 +>> p = p
i +>> p = replicate i ' ' ++> p

-- append a subprog by indenting it
-- with 2 spaces
(+>) :: Prog -> Prog -> Prog
p1 +> p2 = p1 ++ (2 +>> p2)

pr :: String -> Prog
pr     = (:[])

nl :: Prog
nl = [""];

show' :: Show a => a -> Prog
show' = (:[]) . show


toPairs :: (Eq a, Enum a) => [a] -> [(a, a)]
toPairs [] = []
toPairs (x0 : xs0) = toPairs' (x0, x0) xs0
  where
    toPairs' p []   = [p]
    toPairs' p@(l, u) xs@(x : xs1)
      | succ u == x = toPairs' (l, x) xs1
      | otherwise   = p : toPairs xs

pairsToInterval :: Eq a => [(a, a)] -> [Either [a] (a, a)]
pairsToInterval xs
  = case span (\ (l, u) -> l == u) xs of
     ([], [])       -> []
     ([], (y : ys)) -> Right y : pairsToInterval ys
     (xs, ys)       -> Left (map fst xs) : pairsToInterval ys

-- ----------------------------------------
