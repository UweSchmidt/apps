module Main where

-- libraries

import Control.Lens
import Control.Monad

import Data.Maybe ()
import Data.Char
import Data.Map.Strict ( Map )

import System.IO
import Text.Read       ( readMaybe )

-- modules

import Data.Board
import Algorithms.AStar
import Figure
import Puzzles

-- for dev. & test
-- import Data.PriorityQueue.Heap

import qualified Data.Map.Strict as M
import qualified Data.List       as L
-- import qualified Data.Set        as S

-- --------------------

main :: IO ()
main = do
  -- hSetBuffering stdout NoBuffering

  (newpuzzle, gno, b) <- puzzleInput
  if newpuzzle
    then solvePuzzle gno b
    else ownSolution gno b

ownSolution :: Int -> Figure -> IO ()
ownSolution gno b = do
  ms <- readMoves
  case ms of
    Nothing  -> solvePuzzle gno b
    Just mvs -> do
                putStrLn ""
                putStrLn $ printGame b mvs
                saveGame gno b mvs

solvePuzzle :: Int -> Figure -> IO ()
solvePuzzle gno b = do
  putStrLn "\nstart solving game"
  flush
  -- solve the puzzle
  let sol@(mvs, _, _) = solveBoard b
  resOutput sol
  gameOutput b mvs
  saveGame gno b mvs

-- --------------------

flush :: IO ()
flush = hFlush stdout
-- flush = return ()

gameOutput :: Figure -> Path Pos -> IO ()
gameOutput b mvs =
  unless (null mvs) $ do
    ok <- yesNo "play solution"
    when ok $ do
      putStrLn ""
      putStrLn (printGame b mvs)

yesNo :: String -> IO Bool
yesNo msg = do
  putStr $ "\n" ++ msg ++ " (y or n)? : "
  flush
  row <- getLine
  if null row
    then yesNo msg
    else return ((== "y") $ take 1 row)

readMoves :: IO (Maybe [Pos])
readMoves = do
  ok <- yesNo "play own solution"
  if ok
    then do
         putStr "list of moves : " >> flush
         row <- getLine
         if null row
           then readMoves
           else case readMaybe row of
                  Nothing -> readMoves
                  res     -> return res
    else return Nothing

readInt :: String -> IO Int
readInt msg = do
  putStr msg >> flush
  v <- readMaybe <$> getLine
  case v of
    Just i  -> return i
    Nothing -> do
      putStrLn "please input a number"
      readInt msg

resOutput :: (Path Pos, Int, Int) -> IO ()
resOutput (ms, steps, opn)
  | null ms = do
      putStr $
        unlines $ ["sorry, no solution found"] <> stats

  | otherwise = do
      putStr $
        unlines $
        [ "solution with " <> show (length ms) <> " moves found"
        , "list of tiles to play: " <> show ms
        ]
        ++ stats
  where
    stats = [ "statistics:"
            , show steps <> " moves tried"
            , show opn'  <> " moves not tried"
            ]
    opn' | null ms   = opn
         | otherwise = opn - 1

saveGame :: Int -> Figure -> Solution -> IO ()
saveGame no b s =
  maybe save1 save2 $ M.lookup no allPuzzles
  where
    save1 = do
      ok <- yesNo "save board and solution"
      when ok $ do
        saveModulePuzzle (M.insert no (no, b, [s]) allPuzzles)
        putStrLn "puzzle board and solution added to module Puzzles.hs"

    save2 (no', b', ss') = do
      ok <- yesNo "save solution"
      when ok $ do
        saveModulePuzzle (M.insert no' (no', b', ss'') allPuzzles)
        putStrLn "solution added to puzzle in module Puzzles.hs"
          where
            ss''
              | s `elem` ss' = ss'
              | otherwise    = s : ss'

puzzleInput :: IO (Bool, Int, Figure)
puzzleInput = do
  putStr txt1
  flush
  no <- readInt "input figure # : "

  case M.lookup no allPuzzles of
    Just (no', b', _) -> return (False, no', b')

    Nothing -> do
      putStr txt2
      b <- readBoard
      return (True, no, b)

  where
    readBoard :: IO Figure
    readBoard = do
      b' <- validateBoard 5 5 . parseBoard . unlines
            <$>
            traverse read1 prompts
      case b' of
        Right b -> do
          putStrLn "\nboard: "
          putStrLn $ printBoard b
          ok <- yesNo "board o.k."
          if ok
            then return b
            else do
              putStrLn ""
              readBoard
        Left msg -> do
          putStrLn msg
          readBoard

    read1 p = do
      putStr p
      flush
      getLine

    txt1 = unlines
      [ "figure.game: solve puzzle"
      ]
    txt2 = unlines
      [ ""
      , "Input 5 rows with each containing 5 tiles"
      , "top row first, bottom row last"
      , "colors: G (green), R (red), W (white), Y (yellow)"
      , "colors maybe separated by blanks"
      , ""
      ]

    prompts =
      [ "   top row : "
      , "    4. row : "
      , "    3. row : "
      , "    2. row : "
      , "bottom row : "
      ]

-- --------------------
--
-- parse and pretty printing stuff

parseBoard :: String -> Figure
parseBoard =
  view (to parse . to boardList . from isoBoardList)
  where
    parse =
      map (map (view $ from color'char)
           . filter (not . isSpace)
           . map toUpper)
      . reverse
      . lines

    boardList :: [[Color]] -> [(Coord, Color)]
    boardList =
      concat . zipWith ln [1..]
      where
        ln y cs =
          concat $ zipWith co [1..] cs
          where
            co x c = [(V2 x y, c)]

printBoard :: Figure -> String
printBoard = unlines . printBoard'

printBoard' :: Figure -> [String]
printBoard' b = pr 5 5
  where
    pr w h =
      reverse [ln i | i <- [1..h]]
      where
        ln y =
          unwords [toC (V2 j y) | j <- [1..w]]
          where
            toC x = b ^. theBoardAt x . color'char . to (:[])

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

allPuzzles :: Map Int GameF
allPuzzles = M.fromList . concatMap toB $ puzzles
  where
    toB (i, s, ss) =
      either (const []) (\ b -> [(i, (i, b, ss))])
        . validateBoard 5 5
        . parseBoard
        $ s

printAllPuzzles :: Map Int GameF -> String
printAllPuzzles ps =
  unlines $
  header
  ++
  mkl (concatMap pp (M.elems ps))
  ++
  footer
  where
    header =
      [ "module Puzzles"
      , "  ( puzzles )"
      , "where"
      , ""
      , "import Figure"
      , ""
      , "puzzles :: [GameS]"
      , "puzzles ="
      ]
    footer = [""]
    pp (i, b, ss) =
      [ "( " ++ show i
      , ", unlines"
      ]
      ++
      mkl (map qt $ printBoard' b)
      ++
      ( case ss of
          [] ->
            [ ", []"]
          [s1] ->
            [ ", [" <> show s1 <> "]"]
          (s1 : ss1) ->
            [ ", [ " <> show s1]
            <>
            map (\s -> "  , " <> show s) ss1
            <>
            [ "  ]" ]
      )
      ++
      [ ")" ]

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

saveModulePuzzle :: Map Int GameF -> IO ()
saveModulePuzzle pm = do
  old <- readFile mn
  writeFile (mn ++ "~") old
  writeFile mn xs
  where
    xs = printAllPuzzles pm
    -- ??? abs file path
    mn = "/Users/uwe/haskell/apps/figure.game/src/Puzzles.hs"


-- --------------------

{- just for testing

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
{-

#144: Solution in 10 moves rquired
      but only a solution in 11 moves found
      with a weightCost = 0.75
      with a weightCost = 0.80
      a solution in 10 moves was foud: [3,3,3,4,2,5,1,1,1,1]

board:
G W Y Y R
Y Y W W Y
R Y R G G
G Y W G G
W Y G Y R


board o.k. (y or n)? : y

start solving game
solution in 11 moves found
list of tiles to play: [3,4,2,1,1,1,4,1,3,3,5]
statistics:
41756 moves tried
42712 moves not tried

play solution (y or n)? : y

initial board:
G W Y Y R
Y Y W W Y
R Y R G G
G Y W G G
W Y G Y R

1. move: tile 3
G W . Y R
Y Y Y W Y
R Y W G G
G Y R G G
W Y W Y R

2. move: tile 4
G W . . R
Y Y Y Y Y
R Y W W G
G Y R G G
W Y W G R

3. move: tile 2
. . . . .
G . . . R
R . W W G
G . R G G
W W W G R

4. move: tile 1
. . . . .
. . . . R
G . . W G
R . W G G
G . R G R

5. move: tile 1
. . . . .
. . . . R
. . . W G
G . W G G
R . R G R

6. move: tile 1
. . . . .
. . . . R
. . . W G
. . W G G
G . R G R

7. move: tile 4
. . . . .
. . . . .
. . . . .
. . W . R
G . R W R

8. move: tile 1
. . . . .
. . . . .
. . . . .
. . W . R
. . R W R

9. move: tile 3
. . . . .
. . . . .
. . . . .
. . . . R
. . W W R

10. move: tile 3
. . . . .
. . . . .
. . . . .
. . . . R
. . . . R

11. move: tile 5
. . . . .
. . . . .
. . . . .
. . . . .
. . . . .

-}
-- --------------------
