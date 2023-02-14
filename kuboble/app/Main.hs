module Main where

import Data.Char
import Data.Foldable   (traverse_)
import Data.Maybe
import System.Environment (getArgs)
import Text.Printf

import Data.Board
import Kuboble

-- for dev. & test
-- import Data.Maybe
-- import Data.Map.Strict (Map)
-- import Control.Lens
-- import Control.Monad
-- import System.IO

-- import Data.PriorityQueue.Heap
-- import Algorithms.AStar

import qualified Data.List       as L

-- import qualified Data.Map.Strict as M
-- import qualified Data.Set        as S

-- --------------------

main :: IO ()
main = do
  name <- listToMaybe <$> getArgs
  maybe mainAll main1 name

mainAll :: IO ()
mainAll = do
  traverse_ run allPuzzles
  where
    run (name, puzzle) =
      runKubobleSolver name $ parseKuboble puzzle

main1 :: String -> IO ()
main1 name =
  case lookup name allPuzzles of
    Nothing ->
      putStrLn $ "no kuboble puzzle with name " <> show name <> " found"
    Just puzzle ->
      runKubobleSolver name $ parseKuboble puzzle

runKubobleSolver :: String -> Kuboble2 -> IO ()
runKubobleSolver name puzzle =
  putStrLn $ unlines $ showResult name puzzle $ solveKuboble puzzle

-- --------------------
--
-- result output

showResult :: String -> Kuboble2 -> (Solution, (Int, Int, Int)) -> [String]
showResult name puzzle (mvs, stats)
  = concat
    [ ["solving kuboble " <> name, "board:" , ""]
    , map ("  " ++) $ showKuboble2 puzzle
    , [""]
    , sol
    , [""]
    , showStats stats
    ]
  where
    sol = case mvs of
      [] -> ["no solution found"]
      p  -> showSolution p

showBoble :: Boble -> String
showBoble Black = "X"
showBoble White = "."
showBoble (Target Red)   = "r"
showBoble (Target Green) = "g"
showBoble (Target Blue)  = "b"
showBoble (Ball   Red)   = "R"
showBoble (Ball   Green) = "G"
showBoble (Ball   Blue)  = "B"

showSolution :: [Move1] -> [String]
showSolution p =
  [ "solution in " <> show (length p) <> " moves found"
  , ""
  ] <>
  zipWith showStep [1..] p
  where
    showStep :: Int -> Move1 -> String
    showStep i (rgb, ((_c1,_c2), dir)) =
      printf "%2d. " i <> showRGB rgb <> " -> " <> showDir dir

showRGB :: RGB -> String
showRGB Red   = "red  "
showRGB Green = "green"
showRGB Blue  = "blue "

showDir :: Dir -> String
showDir Down   = "down "
showDir Up     = "up   "
showDir Left_  = "left "
showDir Right_ = "right"

showStats :: (Int, Int, Int) -> [String]
showStats (noMoves, noStates, noOpen) =
  [l1, l2, l3]
  where
    l1 = printf "%7d" noMoves  <> " moves tried"
    l2 = printf "%7d" noStates <> " board states checked"
    l3 = printf "%7d" noOpen   <> " moves not yet tried"

showKuboble2 :: Kuboble2 -> [String]
showKuboble2 k2@(K2 tiles' balls') =
  brd
  where
    (V2 xmin ymin, V2 xmax ymax) = bboxBoard (ballsOnBoard k2)

    brd    = reverse $ map row [ymin .. ymax]
    row r  = unwords $ map (\ c -> cell (V2 c r)) [xmin .. xmax]
    cell c = showTile (boardAt c tiles') (boardAt c balls')

    showTile :: Boble -> Boble -> String
    showTile Black        _         = "x"
    showTile White       (Ball rgb) = [toUpper $ showRGB' rgb]
    showTile White        _         = "."
    showTile (Target ct) (Ball cb)  = [toUpper $ showRGB' cb, showRGB' ct]
    showTile (Target ct)  _         = [showRGB' ct]
    showTile _            _         = "x"

showRGB' :: RGB -> Char
showRGB' Red   = 'r'
showRGB' Green = 'g'
showRGB' Blue  = 'b'

-- ----------------------------------------
--
-- parse board and tiles

parseKuboble :: [String] -> Kuboble2
parseKuboble = parseBoard2 . unlines

parseBoard :: String -> Kuboble
parseBoard = shiftOrgKuboble . boardFromList . toBoardList . parse
  where
    parse = map (map charToBoble) . reverse . lines

    toBoardList :: [[Boble]] -> [(Coord, Boble)]
    toBoardList =
      concat . zipWith ln [1..]
      where
        ln y cs =
          concat $ zipWith co [1..] cs
          where
            co x c = [(V2 x y, c)]

parseBoard2 :: String -> Kuboble2
parseBoard2 = shiftOrgKuboble2 . toKuboble2 . toBoardList . parse
  where
    parse = map (map char2ToBoble . words) . reverse . lines

    toKuboble2 :: [(Coord, (Boble, Boble))] -> Kuboble2
    toKuboble2 = L.foldl' ins2 (K2 mempty mempty)
      where
        ins2 (K2 bo ba) (c, (tl, bl)) =
          K2 (setBoardAt c tl bo) (setBoardAt c bl ba)

    toBoardList :: [[(Boble, Boble)]] -> [(Coord, (Boble, Boble))]
    toBoardList =
      concat . zipWith ln [1..]
      where
        ln y cs =
          concat $ zipWith co [1..] cs
          where
            co x c = [(V2 x y, c)]

char2ToBoble :: String -> (Boble, Boble)
char2ToBoble xs =
  swap b1 b2
  where
    (b1 : b2 : _) = map charToBoble $ xs <> "XX"

    swap   (Ball _) y@(Ball _) = (White,  y)
    swap x@(Ball _) y          = swap y x
    swap    Black   y@(Ball _) = (White,  y)
    swap x          y@(Ball _) = (x,      y)
    swap x          _          = (x,  Black)

charToBoble :: Char -> Boble
charToBoble c = case c of
  '.' -> White
  'r' -> Target Red
  'g' -> Target Green
  'b' -> Target Blue
  'R' -> Ball   Red
  'G' -> Ball   Green
  'B' -> Ball   Blue
  _   -> mempty

-- ----------------------------------------
--
-- all puzzles

allPuzzles :: [(String, [String])]
allPuzzles =
  [ ("level1", level1)

  , ("level6", level6)

  , ("level9", level9)
  , ("level10", level10)
  , ("level11", level11)
  , ("level12", level12)

  , ("level19", level19)
  , ("level20", level20)

  , ("level22", level22)
  , ("level23", level23)
  , ("level24", level24)

  , ("level29", level29)

  , ("level39", level39)
  , ("level40", level40)
  , ("level41", level41)

  , ("level45", level45)
  , ("level46", level46)
  ]

-- ----------------------------------------

level3x3 :: [String]       -- 3 x 3
level3x3 =
  [ ". . ."
  , ". . ."
  , ". . ."
  ]

level1 :: [String]       -- 3 x 3
level1 =
  [ "G R ."
  , ". . ."
  , "r g ."
  ]

-- --------------------

level4x3 :: [String]       -- 4 x 3
level4x3 =
  [ ". . . ."
  , ". . . ."
  , ". . . ."
  ]

level6 :: [String]       -- 4 x 3
level6 =
  [ "x g x ."
  , "x r . x"
  , ". . R G"
  ]

level9 :: [String]       -- 4 x 3
level9 =
  [ "x x x x x"
  , "x . R G x"
  , "x . r x x"
  , "g . . . x"
  , "x x x x x"
  ]

level10 :: [String]       -- 4 x 3
level10 =
  [ "x . x r"
  , ". g . ."
  , "x . R G"
  ]

level11 :: [String]       -- 4 x 3
level11 =
  [ "x . . x"
  , ". g r ."
  , "x . R G"
  ]

level12 :: [String]       -- 4 x 3
level12 =
  [ "G Rg . ."
  , "x .  . ."
  , ". .  r ."
  ]


-- --------------------

level4x4 :: [String]       -- 4 x 4
level4x4 =
  [ ". . . ."
  , ". . . ."
  , ". . . ."
  , ". . . ."
  ]

level19 :: [String]       -- 4 x 4
level19 =
  [ "x r x  ."
  , ". . .  ."
  , ". x .  ."
  , ". . Rg G"
  ]

level20 :: [String]       -- 4 x 4
level20 =
  [ "x x . x"
  , ". . g ."
  , ". . . ."
  , ". r R G"
  ]

level22 :: [String]       -- 4 x 4
level22 =
  [ "x . x ."
  , "g . . ."
  , "x r . ."
  , ". . R G"
  ]

level23 :: [String]       -- 4 x 4
level23 =
  [ "x . x ."
  , ". . . ."
  , ". . r ."
  , "g x R G"
  ]

level24 :: [String]       -- 4 x 4
level24 =
  [ "x r x ."
  , ". . g ."
  , ". . . ."
  , ". . R G"
  ]

level29 :: [String]       -- 4 x 4
level29 =
  [ ". x x ."
  , ". x . g"
  , ". r . ."
  , ". . R G"
  ]

-- --------------------

level39 :: [String]       -- 4 x 3  RGB
level39 =
  [ "G R B ."
  , "b . r ."
  , ". . . g"
  ]

level40 :: [String]       -- 4 x 3  RGB
level40 =
  [ "G R Bg ."
  , ". . .  r"
  , ". b .  ."
  ]

level41 :: [String]       -- 4 x 3  RGB
level41 =
  [ "G R B ."
  , ". g . ."
  , ". b . r"
  ]

level45 :: [String]       -- 4 x 3  RGB
level45 =
  [ "r x .  ."
  , ". . b  ."
  , ". B Rg G"
  ]

level46 :: [String]       -- 4 x 3  RGB
level46 =
  [ ". x  . b"
  , ". r  . ."
  , ". Bg R G"
  ]

-- ----------------------------------------
