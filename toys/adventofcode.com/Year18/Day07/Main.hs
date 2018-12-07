-- solution for
-- http://adventofcode.com/2018/day/7


module Main where

import           Data.List
import           Data.Graph (Graph)
import qualified Data.Graph as G
import           Data.Relation (Rel')
import qualified Data.Relation as R
import           Data.Set (Set)
import qualified Data.Set as S
import           Util.Main1 (main12)
import           Control.Arrow (second)
-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-07"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------
--
-- Graph is the right abstraction
-- a Rel does not deal with isolated points

type CRel   = Rel' Char
type CSet   = Set Char
type CGraph = Graph Char

solve1 :: CRel -> [Char]
solve1 = remRoots . G.fromRel . R.trClosure . checkForCycles

remRoots :: CGraph -> [Char]
remRoots g
  | S.null roots = []
  | otherwise    = c1 : remRoots g1
  where
    roots = G.roots g
    c1    = S.findMin roots
    g1    = G.deleteN c1 g

checkForCycles :: CRel -> CRel
checkForCycles r
  | R.acyclic r = r
  | otherwise   = error $ "Graph contains cycles: " ++ show r

-- ----------------------------------------
{-
--
-- first try, works, but too complicated
-- there is a simpler solution: see above

solve1' :: CRel -> [Char]
solve1' r = path1 r $ roots r

-- all start nodes in a graph
roots :: CRel -> CSet
roots r = R.dom tcr `S.difference` R.rng tcr
  where
    tcr = R.trClosure r

noRoots :: CRel -> CSet
noRoots r = R.elems r `S.difference` roots r

path1 :: CRel -> CSet -> [Char]
path1 r = go
  where
    trc      = R.trClosure r
    go available
      | S.null available = []
      | otherwise        =
          -- trace (show (c1, S.toList a1, S.toList c1Succs, S.toList a1Succs)) $
          c1 : go news
      where
        (c1, a1) = S.deleteFindMin available   -- select next node
        c1s1     = r `R.apply` c1              -- comp all succs of next node
        c1Succs  = c1s1                        -- this set must be reduced
                   `S.difference`
                   (noRoots $ R.restrict c1s1 trc)
        a1Succs  = trc `R.applyS` a1
        news     = (c1Succs `S.difference` a1Succs) `S.union` a1

-- hard stuff: when computing new available, it's
-- important in c1Succs and a1Succs to work with the transitive closure of r
-- in both cases, else there show up already processed nodes

-- update: hard stuff if you didn't understand the problem
-}
-- ----------------------------------------

type State   = (Time, Workers, Jobs)
type Workers = [Worker]
type Worker  = Maybe (Job, Time)
type Time    = Int
type Job     = Char
type Jobs    = CSet

solve2 :: CRel -> Int
solve2 = solve2' 61 5

solve2' :: Int -> Int -> CRel -> Int
solve2' dur no r = go 0 workers jobs
  where
    jobs :: CGraph
    jobs = G.fromRel . R.trClosure . checkForCycles $ r

    workers :: Workers
    workers = replicate no Nothing

    go :: Int -> Workers -> CGraph -> Int
    go cnt ws tasks
      | S.null waiting
        &&
        all available ws = cnt
      | otherwise  = -- trace (showSt cnt ws1 waiting) $
                     go cnt' ws' tasks'
      where
        waiting    = G.roots tasks `S.difference` active ws
        ws1        = assignJobs dur (S.toList waiting) ws
        cnt'       = cnt + 1
        (fjs, ws') = termJobs . tick $ ws1
        tasks'     = remTasks tasks fjs

available :: Worker -> Bool
available Nothing = True
available _       = False

active :: Workers -> CSet
active = foldr ins S.empty
  where
    ins Nothing      res = res
    ins (Just (j,_)) res = S.insert j res

termJobs :: Workers -> ([Char], Workers)
termJobs = foldr term ([], [])
  where
    term (Just (c1, 0)) (ts, ws) = (c1 : ts, Nothing : ws)
    term w1             (ts, ws) = (ts,           w1 : ws)

remTasks :: CGraph -> [Char] -> CGraph
remTasks ts = foldr G.deleteN ts

tick :: Workers -> Workers
tick = map decr
  where
    decr = fmap (second (\x -> x - 1))


assignJobs :: Int -> [Char] -> Workers -> Workers
assignJobs _ [] ws = ws                                 -- no more jobs
assignJobs _ _  [] = []                                 -- no more workers
assignJobs dur js@(j1 : js1) (w1 : ws1)
      | Just _ <- w1 = w1 : assignJobs dur js ws1             -- worker active
      | otherwise    = Just (j1, d1) : assignJobs dur js1 ws1 -- worker available
      where
        d1 = fromEnum j1 - fromEnum 'A' + dur

showSt :: Int -> Workers -> CSet -> String
showSt t ws jobs =
  show t ++ ".: [" ++ (intercalate "," $ map js ws) ++ "] " ++ (show $ S.toList jobs)
  where
    js Nothing       = "-"
    js (Just (c, t)) = c : ":" ++ show t

-- ----------------------------------------

fromString :: String -> CRel
fromString = R.fromList . map (toPair . filter ((== 1) . length) . words) . lines
  where
    toPair [(c1 : _), (c2 : _)] = (c1, c2)

ex1 :: CRel
ex1  = fromString ex

inp1 :: CRel
inp1 = fromString inp

ex :: String
ex = unlines $
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
-- , "Step F must be finished before step D can begin."
-- , "Step F must be finished before step B can begin."
  ]


ex2 :: CRel
ex2 = fromString . unlines $
  [ "A C", "A E"        -- E is the trouble maker
  , "C F"               -- E does not become available after A is processed
  , "D E"
  , "E F"
  , "Z C", "Z D"
  ]

inp' :: IO String
inp' = readFile "Year18/Day07/day07.txt"

inp :: String
inp = "Step T must be finished before step C can begin.\nStep V must be finished before step C can begin.\nStep Y must be finished before step H can begin.\nStep R must be finished before step U can begin.\nStep B must be finished before step J can begin.\nStep Q must be finished before step O can begin.\nStep W must be finished before step O can begin.\nStep S must be finished before step X can begin.\nStep I must be finished before step N can begin.\nStep X must be finished before step H can begin.\nStep M must be finished before step L can begin.\nStep A must be finished before step F can begin.\nStep G must be finished before step P can begin.\nStep U must be finished before step E can begin.\nStep Z must be finished before step E can begin.\nStep H must be finished before step L can begin.\nStep P must be finished before step C can begin.\nStep K must be finished before step F can begin.\nStep O must be finished before step C can begin.\nStep C must be finished before step F can begin.\nStep D must be finished before step L can begin.\nStep L must be finished before step F can begin.\nStep N must be finished before step E can begin.\nStep J must be finished before step F can begin.\nStep F must be finished before step E can begin.\nStep I must be finished before step A can begin.\nStep Z must be finished before step J can begin.\nStep I must be finished before step P can begin.\nStep T must be finished before step E can begin.\nStep R must be finished before step F can begin.\nStep U must be finished before step H can begin.\nStep K must be finished before step E can begin.\nStep D must be finished before step N can begin.\nStep U must be finished before step C can begin.\nStep D must be finished before step J can begin.\nStep N must be finished before step F can begin.\nStep C must be finished before step J can begin.\nStep U must be finished before step J can begin.\nStep A must be finished before step O can begin.\nStep H must be finished before step N can begin.\nStep P must be finished before step O can begin.\nStep I must be finished before step E can begin.\nStep G must be finished before step F can begin.\nStep O must be finished before step J can begin.\nStep Q must be finished before step F can begin.\nStep G must be finished before step J can begin.\nStep X must be finished before step E can begin.\nStep S must be finished before step D can begin.\nStep R must be finished before step P can begin.\nStep K must be finished before step L can begin.\nStep R must be finished before step Q can begin.\nStep L must be finished before step N can begin.\nStep Q must be finished before step C can begin.\nStep C must be finished before step D can begin.\nStep C must be finished before step N can begin.\nStep O must be finished before step E can begin.\nStep W must be finished before step F can begin.\nStep K must be finished before step D can begin.\nStep T must be finished before step H can begin.\nStep M must be finished before step D can begin.\nStep Y must be finished before step Z can begin.\nStep J must be finished before step E can begin.\nStep S must be finished before step F can begin.\nStep G must be finished before step U can begin.\nStep V must be finished before step S can begin.\nStep Y must be finished before step F can begin.\nStep G must be finished before step H can begin.\nStep T must be finished before step Q can begin.\nStep S must be finished before step U can begin.\nStep V must be finished before step D can begin.\nStep W must be finished before step M can begin.\nStep M must be finished before step E can begin.\nStep A must be finished before step H can begin.\nStep B must be finished before step F can begin.\nStep B must be finished before step N can begin.\nStep D must be finished before step F can begin.\nStep W must be finished before step K can begin.\nStep P must be finished before step E can begin.\nStep B must be finished before step X can begin.\nStep Q must be finished before step U can begin.\nStep Q must be finished before step X can begin.\nStep X must be finished before step N can begin.\nStep M must be finished before step Z can begin.\nStep G must be finished before step Z can begin.\nStep S must be finished before step G can begin.\nStep P must be finished before step F can begin.\nStep I must be finished before step O can begin.\nStep R must be finished before step A can begin.\nStep L must be finished before step J can begin.\nStep B must be finished before step I can begin.\nStep C must be finished before step E can begin.\nStep B must be finished before step W can begin.\nStep P must be finished before step N can begin.\nStep H must be finished before step C can begin.\nStep K must be finished before step J can begin.\nStep Y must be finished before step M can begin.\nStep Z must be finished before step P can begin.\nStep I must be finished before step K can begin.\nStep V must be finished before step E can begin.\nStep Y must be finished before step P can begin.\nStep T must be finished before step R can begin.\n"
