module Gen
where

import Data.List (sort)
import System.Environment

main :: IO ()
main
    = do args <- options
         let (tiles, size) = uncurry generate args
         putStrLn . show . reverse . sort $ tiles
         putStrLn . show $ size

options :: IO (String, Int)
options
    = do args <- getArgs
         return $
           case args of
             [] -> ("classic", 0)
             (x1 : []) -> (x1, 0)
             (x1 : (x2 : [])) -> (x1, read x2)




type Problem   = ([Int], Int)
type Generator =  Int -> Problem

generate :: String -> Int -> Problem
generate s
    = maybe classic id . lookup s $ generators


generators :: [(String, Generator)]
generators
    = [ ("classic",     classic)
      , ("classic2",    classic2)
      , ("simple",      simple)
      , ("imperfect23", imperfect23)
      , ("perfect110",  perfect110)
      , ("perfect112",  perfect112)
      ]

classic :: Generator
classic n
    | n > 0
        = ([1 .. (n - 1)], n)
    | otherwise
        = classic 20

classic2 :: Generator
classic2 n
    | n > 1
        = ([1 .. n `div` 2], n)
    | otherwise
        = classic2 20

simple :: Generator
simple n
    | n <= 0
        = simple 1
    | otherwise
        = ((1 :) . concat . replicate 3 . map (2^) $ [0..n-1], 2^n)

imperfect23 :: Generator
imperfect23 n
    | n == 1				-- imperfect 23 with a 1x1 hole
        = ([12,11,11,7,5,5,4,3,3,2,2,1], 23)
    | n == 2				-- imperfect 23 with all squares
        = ([11,5,3,2,1] ++ [1..22], 23)
    | otherwise				-- imperfect 23
        = ([12,11,11,7,5,5,4,3,3,2,2,1,1], 23)

imperfect23WithHole :: Generator
imperfect23WithHole __
    = ([12,11,11,7,5,5,4,3,3,2,2,1], 23)

perfect112 :: Generator
perfect112 n
    | n == 1
        = ([50,35,27,8,19,15,17,11,6,24,2,9,25,29,7,18,16,42,37,4,33,1],112) 
    | n == 2
        = ([1..111],112) 
    | otherwise
        = ([50,35,27,8,19,15,17,11,6,24,2,9,25,29,7,18,16,42,37,4,33],112) 

perfect110 :: Generator
perfect110 n
    | n == 1
        = ([2,3,4,6,7,8,12,13,14,15,16,17,18,21,22,23,24,26,27,28,50,60],110)
    | n == 2
        = ([1..109],110) 
    | otherwise
        = ([1,2,3,4,6,7,8,12,13,14,15,16,17,18,21,22,23,24,26,27,28,50,60],110)

perfect308 :: Generator
perfect308 n
    | n == 1
        = ([1,2,3,4,7,10,12,13,23,25,34,37,38,39,43,44,45,62,77,79,85,87,108,113,115,116],308)
    | n == 2
        = ([1..307],308) 
    | otherwise
        = ([1,3,4,7,10,12,13,23,25,34,37,38,39,43,44,45,62,77,79,85,87,108,113,115,116],308)
