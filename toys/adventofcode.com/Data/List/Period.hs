module Data.List.Period
  (findPeriod)
where

-- compute a prefix and a period of a list of values
-- which looks like it has a periodical structure

import           Data.List  (foldl', isPrefixOf)

import           Control.Arrow (first) -- , second, (***))

-- ----------------------------------------

findPeriod :: Eq a => [a] -> Maybe ([a], [a])
findPeriod xs
  | null ps   = Nothing
  | otherwise = Just res
  where
    res@(_px, ps) = findPeriod' xs

findPeriod' :: Eq a => [a] -> ([a], [a])
findPeriod' xs
  | inv = (px, ps)
  | otherwise = error "findPeriod: wrong result"
  where
    (sp, xp) = findPeriodRev $ reverse xs
    ps = reverse sp  -- the period found
                     -- _n: # times the period occurs in xs, must be >= 2
    px = reverse xp  -- the prefix of xs, not part of the period

    inv = xs `isPrefixOf` (px ++ concat (repeat ps))

-- solution: all pairs (px, ps) with px ++ ps == xs
-- are candiadates (not . null $ zs)
-- if ps is suffix of px, ps occurs at least 2 times
-- so it may be seen as a period
--
-- but a solution may not be unique:
--
-- example:    [1,2,3,4,4,5,5,4,4,5,5,4,4,5,5,4,4,5,5]
--
-- solutions: .1 ([1,2,3,4,4,5,5,4,4,5,5,4,4,5,5,4,4],([5], 2))
--            .2 ([1,2,3],([4,4,5,5], 4))
--            .3 ([1,2,3],([4,4,5,5,4,4,5,5], 2))
--
-- .1 the prefix is too long
-- .2 is the solution seached for
-- .3 the period contains a repetition
--
-- .2 is better than .1: px is shorter
-- .2 is better than .3: period is shorter
-- search becomes easier, when list is reversed

findPeriodRev :: Eq a => [a] -> ([a], [a])
findPeriodRev xs =
  uncurry shortenPX .
  maybe ([], xs) (first snd) .
  bestPeriod   .
  toCandidates .
  prefixSuffix
  $ xs

prefixSuffix :: [a] -> [([a], [a])]
prefixSuffix []            = [([], [])]
prefixSuffix xs@(x1 : xs1) =
  ([], xs) : map (first (x1:)) (prefixSuffix xs1)

reps :: Eq a => [a] -> [a] -> ((Int, [a]), [a])
reps [] ys = ((1, []), ys )
reps xs ys = ((n, xs), ys1)
  where
    lenXS    = length xs
    (n, ys1) = cntPX 1 ys
    cntPX i ys'
      | xs `isPrefixOf` ys' = cntPX (i + 1) (drop lenXS ys')
      | otherwise           = (i, ys')

toCandidates :: Eq a => [([a], [a])] -> [((Int, [a]), [a])]
toCandidates = filter ((> 1) . fst . fst) . map (uncurry reps)

bestPeriod :: [((Int, [a]), [a])] -> Maybe ((Int, [a]), [a])
bestPeriod []        = Nothing
bestPeriod (x1 : xs) = Just $ foldl' maxP x1 xs
  where
    maxP p1@((i1, ps1), px1) p2@((i2, ps2), px2)
      | lx1 < lx2 = p1
      | lx1 > lx2 = p2
      | -- lx1 == lx2 &&
        i1  >= i2 = p1
      | otherwise = p2
      where
        lx1 = length px1
        lx2 = length px2

shortenPX :: Eq a => [a] -> [a] -> ([a], [a])
shortenPX (p1 : ps) (x1 : px)
  | p1 == x1 = shortenPX (ps ++ [p1]) px
shortenPX ps px = (ps, px)

-- ----------------------------------------
--
-- some test cases

-- generate digit sequence for rational numbers

rat2digits :: Int -> Int -> [Int]
rat2digits x y
  | x >= 10 * y =     rat2digits x (y * 10)
  | otherwise   = q : rat2digits (r * 10) y
    where
      (q, r) = x `divMod` y


_7'13
  , _7'133 :: [Int]
_7'13  = take 100 $ rat2digits 7 13
_7'133 = take 100 $ rat2digits 7 133


t1, t2 :: Maybe ([Int], [Int])
t1 = findPeriod _7'13
t2 = findPeriod _7'133

t3 :: Maybe ([(Int, Int)], [(Int, Int)])
t3 = findPeriod gen600

-- solution from solve2 in Year18/Day18

gen600 :: [(Int, Int)]
gen600 = [(451,531),(596,445),(770,344),(926,319),(1061,311),(1160,348),(1226,383),(1245,449),(1225,505),(1169,568),(1073,602),(967,586),(856,557),(727,563),(618,509),(543,411),(460,383),(401,304),(361,244),(326,208),(293,194),(265,173),(242,165),(222,157),(215,138),(214,123),(216,122),(217,127),(225,113),(235,119),(247,113),(257,124),(271,129),(286,142),(298,153),(315,163),(336,169),(353,185),(376,191),(400,202),(416,216),(431,237),(452,245),(460,267),(465,280),(471,301),(474,294),(473,302),(471,304),(455,317),(432,320),(416,315),(401,304),(398,277),(401,267),(406,255),(408,253),(421,234),(432,240),(446,232),(466,240),(479,256),(487,270),(499,278),(516,287),(513,305),(515,308),(523,303),(527,313),(541,317),(545,335),(558,306),(576,315),(583,329),(574,353),(560,361),(541,376),(512,376),(487,367),(464,358),(447,322),(434,310),(429,284),(423,285),(420,265),(427,244),(433,241),(439,233),(441,239),(450,245),(453,256),(455,265),(466,261),(476,261),(485,268),(487,268),(484,272),(482,282),(480,289),(481,292),(481,298),(482,285),(490,249),(493,256),(487,270),(482,281),(471,292),(460,299),(463,290),(475,276),(482,277),(496,266),(511,270),(524,281),(538,291),(540,293),(541,298),(538,308),(529,320),(517,325),(508,322),(507,314),(509,308),(510,296),(508,293),(510,291),(510,294),(506,298),(506,298),(505,292),(499,286),(493,287),(491,285),(488,286),(484,288),(481,289),(481,284),(485,274),(494,271),(504,272),(512,278),(525,277),(534,286),(541,296),(553,302),(565,306),(569,317),(573,316),(575,326),(572,342),(565,353),(560,344),(560,339),(560,337),(560,329),(559,326),(565,309),(564,315),(554,320),(545,316),(531,317),(519,315),(510,306),(498,308),(492,297),(496,280),(500,276),(508,273),(514,275),(523,271),(526,282),(535,282),(540,290),(549,295),(558,305),(566,308),(573,321),(583,325),(586,338),(590,346),(594,347),(596,354),(597,359),(600,351),(603,346),(598,345),(588,339),(580,331),(563,343),(546,345),(529,338),(515,324),(506,314),(506,293),(506,290),(508,284),(508,280),(511,274),(511,277),(511,278),(508,278),(508,282),(509,286),(513,290),(520,292),(525,296),(531,296),(538,300),(547,303),(554,317),(565,318),(578,318),(593,319),(599,325),(596,327),(592,336),(584,350),(573,358),(565,357),(558,352),(559,336),(564,319),(571,316),(574,315),(573,313),(570,316),(564,319),(555,323),(548,320),(541,312),(534,311),(530,307),(528,308),(528,309),(535,303),(547,298),(556,300),(568,305),(579,311),(588,319),(596,329),(602,337),(604,336),(606,341),(601,348),(594,351),(585,356),(576,360),(566,360),(567,341),(570,332),(578,323),(579,318),(580,312),(572,319),(565,319),(548,326),(538,317),(528,316),(525,306),(522,308),(525,301),(530,301),(541,296),(553,298),(568,303),(582,308),(591,314),(600,321),(603,331),(602,334),(597,344),(591,354),(590,351),(587,351),(581,352),(576,346),(577,330),(575,334),(573,335),(571,333),(572,322),(571,319),(571,314),(564,315),(558,314),(551,318),(546,317),(542,319),(541,315),(541,313),(547,306),(553,306),(562,308),(574,306),(587,309),(603,316),(614,321),(614,327),(612,333),(604,347),(596,355),(585,361),(576,364),(576,350),(583,332),(588,329),(592,328),(593,328),(594,329),(588,339),(584,339),(576,334),(571,327),(564,328),(556,325),(549,325),(547,319),(543,318),(546,308),(550,306),(558,303),(567,305),(578,303),(591,308),(601,312),(605,318),(610,325),(610,337),(605,343),(598,347),(588,353),(583,351),(581,340),(584,338),(593,332),(601,329),(607,326),(607,330),(605,334),(596,341),(584,349),(573,353),(565,342),(559,334),(557,325),(553,322),(553,312),(554,310),(558,309),(565,304),(573,303),(585,303),(594,307),(596,312),(598,317),(596,328),(596,334),(595,339),(593,343),(592,340),(597,325),(600,330),(606,331),(605,338),(608,337),(611,337),(613,338),(607,339),(600,341),(592,346),(583,347),(571,352),(565,346),(561,337),(564,321),(564,316),(566,314),(570,310),(575,307),(582,311),(590,305),(592,308),(593,312),(591,323),(589,329),(586,332),(582,337),(585,332),(594,320),(603,321),(612,322),(619,323),(623,326),(624,334),(620,342),(612,350),(609,349),(606,348),(600,345),(592,342),(587,341),(580,344),(577,340),(576,340),(575,330),(577,322),(581,315),(586,315),(589,316),(591,305),(591,311),(588,321),(585,325),(580,329),(575,333),(575,327),(580,313),(587,313),(596,313),(601,315),(607,314),(610,320),(612,324),(611,329),(611,329),(611,334),(612,335),(617,338),(621,341),(622,341),(624,339),(625,340),(624,348),(622,351),(617,354),(617,347),(615,344),(610,337),(603,339),(595,341),(591,333),(585,333),(580,335),(579,329),(584,314),(589,315),(596,313),(599,315),(604,313),(605,320),(606,322),(604,324),(604,322),(604,327),(605,327),(607,330),(609,331),(610,333),(614,330),(617,333),(620,337),(625,336),(628,339),(634,340),(637,341),(634,341),(630,344),(623,350),(616,352),(608,350),(600,351),(596,344),(598,327),(601,325),(606,322),(609,321),(614,317),(616,321),(618,322),(617,324),(617,326),(617,331),(618,331),(620,334),(623,335),(625,337),(630,334),(634,337),(638,341),(643,342),(646,345),(651,348),(653,349),(649,349),(644,352),(636,358),(628,360),(618,358),(608,359),(602,352),(602,336),(603,333),(607,327),(609,325),(614,317),(616,321),(618,322),(617,324),(617,326),(617,331),(618,331),(620,334),(623,335),(625,337),(630,334),(634,337),(638,341),(643,342),(646,345),(651,348),(653,349),(649,349),(644,352),(636,358),(628,360),(618,358),(608,359),(602,352),(602,336),(603,333),(607,327),(609,325),(614,317),(616,321),(618,322),(617,324),(617,326),(617,331),(618,331),(620,334),(623,335),(625,337),(630,334),(634,337),(638,341),(643,342),(646,345),(651,348),(653,349),(649,349),(644,352),(636,358),(628,360),(618,358),(608,359),(602,352),(602,336),(603,333),(607,327),(609,325),(614,317),(616,321),(618,322),(617,324),(617,326),(617,331),(618,331),(620,334),(623,335),(625,337),(630,334),(634,337),(638,341),(643,342),(646,345),(651,348),(653,349),(649,349),(644,352),(636,358),(628,360),(618,358),(608,359),(602,352),(602,336),(603,333),(607,327),(609,325),(614,317),(616,321),(618,322),(617,324),(617,326),(617,331),(618,331),(620,334),(623,335),(625,337),(630,334)]