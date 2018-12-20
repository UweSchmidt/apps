{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/15

module Main where

import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set        as S
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.List
import           Util.Main1 (main12)

import           Control.Arrow (first, second)
import           Control.Monad.State.Strict
import           Control.Monad.Except

-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-15"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = toString . solve1 . fromString

captcha2 :: String -> String
captcha2 = toString . solve2 . fromString

-- ----------------------------------------

type Point      = (Int, Int)
type Ps         = Set Point

data Board      = B { walls   :: Ps
                    , elfs    :: Ps
                    , goblins :: Ps
                    , rounds  :: Int
                    }
                deriving (Show)

emptyBoard :: Board
emptyBoard = B { walls   = emptyPs
               , elfs    = emptyPs
               , goblins = emptyPs
               , rounds  = 0
               }

emptyPs :: Ps
emptyPs = S.empty

singlePs :: Point -> Ps
singlePs = S.singleton

u :: Ps -> Ps -> Ps
u = S.union

-- ----------------------------------------

type Action     = ExceptT String (State Board)

runAction :: Action a -> Board -> (Either String a, Board)
runAction action board0 =
  runState (runExceptT action) board0

incrRounds :: Action ()
incrRounds = modify (\ s -> s { rounds = rounds s + 1})

getWalls :: Action Ps
getWalls = gets walls

getElfs :: Action Ps
getElfs = gets elfs

getGoblins :: Action Ps
getGoblins = gets goblins

getUnits :: Action Ps
getUnits = gets (\ s -> elfs s `u` goblins s)

getClosed :: Action Ps
getClosed = gets (\ s -> walls s `u` elfs s `u` goblins s)

round :: Action ()
round = do
  incrRounds
  ps <- getUnits
  mapM_ processUnit cs

processUnit :: Point -> Action ()
processUnit p = undefined

-- ----------------------------------------

solve1 :: (Ps, Ps, Ps) -> Board
solve1 (ws, es, gs) = snd . runAction (forever round) $ b0
  where
    b0 = emptyboard
         { walls   = ws
         , elfs    = es
         , goblins = gs
         }

solve2 :: (Ps, Ps, Ps) -> Board
solve2 (ws, es, gs) = snd . runAction (return ()) $ b0
  where
    b0 = emptyboard
         { walls   = ws
         , elfs    = es
         , goblins = gs
         }

-- ----------------------------------------

toString :: Point -> String
toString (x, y) = show x ++ "," ++ show y

fromString :: String -> (Ps, Ps, Ps)
fromString = undefined

ex :: String
ex = unlines
  [
  ]

inp' :: IO String
inp' = readFile "Year18/Day15/day15.txt"

inp :: String
inp = ""
