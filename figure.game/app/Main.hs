module Main where

import Data.Maybe
import Data.Char
import Data.Map.Strict ( Map )
import Control.Monad
import System.IO

import Data.Board
import Algorithms.AStar
import Figure
import Puzzles

-- for dev. & test
import Data.PriorityQueue.List
import Control.Lens

import qualified Data.Map.Strict as M
import qualified Data.List       as L
import qualified Data.Set        as S

-- --------------------

main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering

  (no, board) <- puzzleInput
  putStrLn "\nstart solving game"
  flush

  let sol@(mvs, _, _) = solveBoard board
  resOutput $ solveBoard board
  gameOutput board mvs
  saveGame no board
  return ()

-- --------------------

--- flush = hFlush stdout
flush = return ()

gameOutput :: Figure -> Path Pos -> IO ()
gameOutput b mvs =
  when (not (null mvs)) $ do
    ok <- yesNo "play solution"
    when ok $ do
      putStrLn ""
      putStrLn (printGame b mvs)

yesNo :: String -> IO Bool
yesNo msg = do
  putStr $ "\n" ++ msg ++ " (y or n)? : "
  flush
  (== "y") . take 1 <$> getLine


resOutput :: (Path Pos, Int, Int) -> IO ()
resOutput (ms, steps, opn)
  | null ms = do
      putStr $
        unlines $
        [ "sorry, no solution found"
        ]
        ++ stats

  | otherwise = do
      putStr $
        unlines $
        [ "solution in " ++ show (length ms) ++ " moves found"
        , "list of tiles to play: " ++ show ms
        ]
        ++ stats
  where
    stats = [ "statistics:"
            , show steps ++ " moves tried"
            , show opn ++ " moves not tried"
            ]

saveGame :: Int -> Figure -> IO ()
saveGame no b =
  when (not $ M.member no allPuzzles) $ do
    ok <- yesNo "save board"
    when ok $ do
      saveModulePuzzle (M.insert no b allPuzzles)
      putStrLn "puzzle added to module Puzzles.hs"

puzzleInput :: IO (Int, Figure)
puzzleInput = do
  putStr txt1
  flush
  putStr "input figure # : "
  flush

  no <- read <$> getLine
  case M.lookup no allPuzzles of
    Just b -> return (no, b)

    Nothing -> do
      putStr txt2
      board <- readBoard
      return (no, board)

  where
    readBoard :: IO Figure
    readBoard = do
      b <- parseBoard . unlines <$> traverse read1 prompts
      putStrLn "\nboard: "
      putStrLn $ printBoard b
      ok <- yesNo "board o.k."
      if ok
        then return b
        else do
          putStrLn ""
          readBoard

    read1 p = do
      putStr p
      flush
      getLine

    txt1 = unlines
      [ "figure.game: solve new puzzle"
      ]
    txt2 = unlines
      [ ""
      , "Input 5 lines with each containing 5 tiles"
      , "top line first, bottom line last"
      , "colors: G (green), R (red), W (white), Y (yellow)"
      , "colors maybe separated by blanks"
      , ""
      ]

    prompts =
      [ "top line   : "
      , "4. line    : "
      , "3. line    : "
      , "2. line    : "
      , "bottom line: "
      ]

-- --------------------
--
-- parse and pretty printing stuff

parseBoard :: String -> Figure
parseBoard = Board . M.fromList . toBoardList . parse
  where
    parse =
      map (map toColor . filter isColor . map toUpper) . reverse . lines
      where
        isColor = (`elem` ("GRWY" :: String))
        toColor = read . (:[]) . toUpper

    toBoardList :: [[Color]] -> [(Coord, Color)]
    toBoardList =
      concat . zipWith ln [1..]
      where
        ln y cs =
          concat $ zipWith co [1..] cs
          where
            co x c = [(V2 x y, c)]

printBoard :: Figure -> String
printBoard = unlines . printBoard'

printBoard' :: Figure -> [String]
printBoard' (Board m) = pr 5 5
  where
    pr w h =
      reverse [ln i | i <- [1..h]]
      where
        ln y =
          unwords [toC j | j <- [1..w]]
          where
            toC x = maybe "." show . M.lookup (V2 x y) $ m

printNextBoards :: [(Pos, Figure)] -> String
printNextBoards = L.intercalate "\n" . map f1
  where
    f1 (m, b) =
      "move: " ++ show m ++ "\n"
      ++
      printBoard b
      ++
      "clusters: " ++ show (numberOfClusters b) ++ "\n"

printGame :: Figure -> Path Pos -> String
printGame b0 p0 = concatMap step $ playFigure b0 p0
  where
    step (i, p, b) =
      line1 ++ "\n"
      ++
      printBoard b
      ++
      "\n"
      where
        line1
          | i == 0 = "initial board:"
          | otherwise = show i ++ ". move: tile " ++ show p

-- --------------------

allPuzzles :: Map Int Figure
allPuzzles = M.fromList . map toB $ puzzles
  where
    toB (i, s) = (i, parseBoard s)

printAllPuzzles :: Map Int Figure -> String
printAllPuzzles ps =
  unlines $
  header
  ++
  mkl (concatMap pp (M.toList ps))
  ++
  footer
  where
    header =
      [ "module Puzzles"
      , "  ( puzzles )"
      , "where"
      , ""
      , "puzzles :: [(Int, String)]"
      , "puzzles ="
      ]
    footer = [""]
    pp (i, b) =
      [ "( " ++ show i
      , ", unlines"
      ]
      ++
      mkl (map qt $ printBoard' b)
      ++
      [ ")"]

mkl :: [String] -> [String]
mkl ls = zipWith prep il ls ++ ["  ]"]
  where
    il = "  [ " : repeat "  , "

    prep _ps ""          = ""

    prep _ps xs@(x1 : _)
      | x1 `elem` chars  = "    " ++ xs
      where
        chars :: String
        chars = " , )]"

    prep ps xs           = ps     ++ xs

qt :: String -> String
qt = ("\"" ++) . (++ "\"")

saveModulePuzzle :: Map Int Figure -> IO ()
saveModulePuzzle pm = do
  old <- readFile mn
  writeFile (mn ++ "~") old
  writeFile mn xs
  where
    xs = printAllPuzzles pm
    -- ??? abs file path
    mn = "/Users/uwe/haskell/apps/figure.game/src/Puzzles.hs"


-- --------------------

-- {- just for testing

s1 :: String  -- figure #75
s1 = unlines $
  [ "R R R W W"
  , "G W W G R"
  , "W Y W Y G"
  , "R Y Y R R"
  , "R Y G G Y"
  ]

s2 :: String  -- figure #76, 11 moves
s2 = unlines $
  [ "R G W W R"
  , "W W W G Y"
  , "R R R Y W"
  , "Y R R Y R"
  , "G R Y G W"
  ]

s3 :: String  -- figure #78, 9 moves
s3 = unlines $
  [ "G Y W G Y"
  , "Y W W G Y"
  , "W G G W W"
  , "W G Y G R"
  , "W W G R G"
  ]

s4 :: String -- figure #77, 10 moves
s4 = unlines $
  [ "G Y G G W"
  , "Y W W G Y"
  , "Y R W Y W"
  , "G R Y Y W"
  , "R Y W R Y"
  ]

s5 :: String -- figure #79, 10 moves
s5 = unlines $
  [ "Y W G G R"
  , "Y R R G G"
  , "G Y G W W"
  , "W G G W G"
  , "W G W R W"
  ]

s6 :: String
s6 = unlines $
  [ "W W W W W"
  , "G G G G G"
  , "W W W W W"
  , "Y Y Y Y Y"
  , "G R G R G"
  ]

s7 :: String
s7 = unlines
  [ "G G G G G"
  , "R R R R R"
  , "W W W W W"
  , "Y Y Y Y Y"
  , "G R W Y G"
  ]


(b1, i1, p1, c1, n1, pb1, nbs1, ass1) = fff s1
(b2, i2, p2, c2, n2, pb2, nbs2, ass2) = fff s2
(b3, i3, p3, c3, n3, pb3, nbs3, ass3) = fff s3
(b4, i4, p4, c4, n4, pb4, nbs4, ass4) = fff s4
(b5, i5, p5, c5, n5, pb5, nbs5, ass5) = fff s5
(b6, i6, p6, c6, n6, pb6, nbs6, ass6) = fff s6
(b7, i7, p7, c7, n7, pb7, nbs7, ass7) = fff s7

fff s = (b, i, p, c, n, pb, nbs, ass)
  where
    b = parseBoard s
    i = invertBoard b
    p = partBoard i
    c = clusters p
    n = nextMoves' p
    pb = printBoard b
    nbs = nextBoards b
    ass = initBoardAStar b
-- -}
-- --------------------
