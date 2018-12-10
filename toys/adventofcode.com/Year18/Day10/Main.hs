{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/9

module Main where

import           Data.Char
import           Data.IntMap (IntMap)
import qualified Data.IntMap as M
import           Data.List
import           Util.Main1 (main12)
import           Control.Arrow (second)
-- import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-10"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

type Point      = (Int, Int)
type Points     = [Point]
type Velocities = [Point]

zipp :: (Int -> Int -> Int) -> Point -> Point -> Point
zipp op (x1, y1) (x2, y2) = (x1 `op` x2, y1 `op` y2)

addp = zipp (+)
subp = zipp (-)
minp = zipp min
maxp = zipp max

bbox :: Points -> (Point, Point)
bbox = foldl' mmx ((maxBound, maxBound), (minBound, minBound))
  where
    mmx (p1, p2) p = (p1 `minp` p, p2 `maxp` p)

toOrig :: Points -> Points
toOrig ps = ps'
  where
    p1  = fst $ bbox ps
    ps' = map (`subp` p1) ps

boxSize :: (Point, Point) -> Int
boxSize (p1, p2) = uncurry (*) $ p2 `subp` p1

move :: Velocities -> Points -> Points
move = zipWith addp

moves :: Velocities -> Points -> [Points]
moves vs = iterate (move vs)

minSize :: Num a => [a] -> [a]
minSize xs =  zipWith (-) xs (tail xs)

solve1 :: (Points, Velocities) -> String
solve1 (ps, vs) = unlines . toLines . toOrig $ solution
  where
    mvs      = moves vs ps
    difSizes = minSize . map (boxSize . bbox) $ mvs

    solution :: Points
    solution = snd . head . dropWhile ((> 0) . fst) $ zip difSizes mvs

solve2 :: (Points, Velocities) -> Int
solve2 (ps, vs) = length . takeWhile ((> 0) . fst) $ zip difSizes mvs
  where
    mvs      = moves vs ps
    difSizes = minSize . map (boxSize . bbox) $ mvs

-- ----------------------------------------

toLines :: Points -> [String]
toLines ps = [ line y | y <- [y1 .. y2] ]
  where
    line y = [ toC x | x <- [x1 .. x2]]
      where
        toC x = b2c $ (x, y) `elem` ps
        b2c False = '.'
        b2c True  = '#'

    ((x1, y1), (x2, y2)) = bbox ps

fromString :: String -> (Points, Velocities)
fromString = unzip . map toP . lines
  where
    toP l0 = ((read x, read y), (read vx, read vy))
      where
        (x,  l1) = span (/= ',') . drop 1 . snd . span (/= '<') $ l0
        (y,  l2) = span (/= '>') . drop 1 $ l1
        (vx, l3) = span (/= ',') . drop 1 . snd . span (/= '<') $ l2
        (vy, _ ) = span (/= '>') . drop 1 $ l3

-- ----------------------------------------

-- "CRXKEZPZ"
res :: IO ()
res = putStrLn $ solve1 $ fromString inp

ex :: String
ex = unlines
  [ "position=< 9,  1> velocity=< 0,  2>"
  , "position=< 7,  0> velocity=<-1,  0>"
  , "position=< 3, -2> velocity=<-1,  1>"
  , "position=< 6, 10> velocity=<-2, -1>"
  , "position=< 2, -4> velocity=< 2,  2>"
  , "position=<-6, 10> velocity=< 2, -2>"
  , "position=< 1,  8> velocity=< 1, -1>"
  , "position=< 1,  7> velocity=< 1,  0>"
  , "position=<-3, 11> velocity=< 1, -2>"
  , "position=< 7,  6> velocity=<-1, -1>"
  , "position=<-2,  3> velocity=< 1,  0>"
  , "position=<-4,  3> velocity=< 2,  0>"
  , "position=<10, -3> velocity=<-1,  1>"
  , "position=< 5, 11> velocity=< 1, -2>"
  , "position=< 4,  7> velocity=< 0, -1>"
  , "position=< 8, -2> velocity=< 0,  1>"
  , "position=<15,  0> velocity=<-2,  0>"
  , "position=< 1,  6> velocity=< 1,  0>"
  , "position=< 8,  9> velocity=< 0, -1>"
  , "position=< 3,  3> velocity=<-1,  1>"
  , "position=< 0,  5> velocity=< 0, -1>"
  , "position=<-2,  2> velocity=< 2,  0>"
  , "position=< 5, -2> velocity=< 1,  2>"
  , "position=< 1,  4> velocity=< 2,  1>"
  , "position=<-2,  7> velocity=< 2, -2>"
  , "position=< 3,  6> velocity=<-1, -1>"
  , "position=< 5,  0> velocity=< 1,  0>"
  , "position=<-6,  0> velocity=< 2,  0>"
  , "position=< 5,  9> velocity=< 1, -2>"
  , "position=<14,  7> velocity=<-2,  0>"
  , "position=<-3,  6> velocity=< 2, -1>"
  ]

inp' :: IO String
inp' = readFile "Year18/Day10/day10.txt"

inp = "position=<-40181, -50237> velocity=< 4,  5>\nposition=<-40122,  30405> velocity=< 4, -3>\nposition=<-40158, -50246> velocity=< 4,  5>\nposition=<-50220,  50573> velocity=< 5, -5>\nposition=< 20317, -30082> velocity=<-2,  3>\nposition=<-50207,  40490> velocity=< 5, -4>\nposition=< 10260,  -9913> velocity=<-1,  1>\nposition=<-50212, -20003> velocity=< 5,  2>\nposition=< 40498, -40158> velocity=<-4,  4>\nposition=< 30446,  20330> velocity=<-3, -2>\nposition=< 10255, -50241> velocity=<-1,  5>\nposition=< 10263, -40158> velocity=<-1,  4>\nposition=<-40178,  40492> velocity=< 4, -4>\nposition=< 40487,  30411> velocity=<-4, -3>\nposition=<-19978,  30411> velocity=< 2, -3>\nposition=<-50215, -40162> velocity=< 5,  4>\nposition=< 40474, -30078> velocity=<-4,  3>\nposition=< 20309,  10241> velocity=<-2, -1>\nposition=<-50235,  40484> velocity=< 5, -4>\nposition=< 10259, -50246> velocity=<-1,  5>\nposition=< 10228,  10248> velocity=<-1, -1>\nposition=<-19970,  40487> velocity=< 2, -4>\nposition=<-30093, -50246> velocity=< 3,  5>\nposition=< -9929,  -9918> velocity=< 1,  1>\nposition=< 20307, -50246> velocity=<-2,  5>\nposition=< 30417, -19994> velocity=<-3,  2>\nposition=< 10279,  40491> velocity=<-1, -4>\nposition=<-30089, -19997> velocity=< 3,  2>\nposition=<-40132,  -9922> velocity=< 4,  1>\nposition=<-40132,  10244> velocity=< 4, -1>\nposition=< 10234, -50246> velocity=<-1,  5>\nposition=< 10247,  50569> velocity=<-1, -5>\nposition=<-20012,  50572> velocity=< 2, -5>\nposition=< 20348, -50243> velocity=<-2,  5>\nposition=< 40506,  -9922> velocity=<-4,  1>\nposition=<-30067,  30411> velocity=< 3, -3>\nposition=< -9928,  30402> velocity=< 1, -3>\nposition=< 40490,  40492> velocity=<-4, -4>\nposition=<-50247, -40156> velocity=< 5,  4>\nposition=<-40182, -40158> velocity=< 4,  4>\nposition=< 10282,  40492> velocity=<-1, -4>\nposition=<-40169, -50245> velocity=< 4,  5>\nposition=< 30436,  10244> velocity=<-3, -1>\nposition=<-30042,  30406> velocity=< 3, -3>\nposition=< 10236, -50244> velocity=<-1,  5>\nposition=< 40523, -50246> velocity=<-4,  5>\nposition=<-30101,  -9916> velocity=< 3,  1>\nposition=< 50571, -50238> velocity=<-5,  5>\nposition=<-19980, -40157> velocity=< 2,  4>\nposition=<-30092,  30406> velocity=< 3, -3>\nposition=<-20009,  10244> velocity=< 2, -1>\nposition=< 30433,  30411> velocity=<-3, -3>\nposition=< 40514, -50242> velocity=<-4,  5>\nposition=< 40479, -30075> velocity=<-4,  3>\nposition=<-20020,  50566> velocity=< 2, -5>\nposition=< 20309, -30076> velocity=<-2,  3>\nposition=<-50237,  20324> velocity=< 5, -2>\nposition=< 20341, -40165> velocity=<-2,  4>\nposition=< -9919,  30409> velocity=< 1, -3>\nposition=< 50555,  30405> velocity=<-5, -3>\nposition=<-50205,  30402> velocity=< 5, -3>\nposition=< 20365,  40485> velocity=<-2, -4>\nposition=< -9939,  30407> velocity=< 1, -3>\nposition=< 30406,  30410> velocity=<-3, -3>\nposition=< 40527,  30411> velocity=<-4, -3>\nposition=< 20339, -40156> velocity=<-2,  4>\nposition=<-20012,  -9922> velocity=< 2,  1>\nposition=<-30049,  30406> velocity=< 3, -3>\nposition=<-20020,  30407> velocity=< 2, -3>\nposition=< 20330, -50240> velocity=<-2,  5>\nposition=< 20338,  40487> velocity=<-2, -4>\nposition=< 40526,  50567> velocity=<-4, -5>\nposition=<-30040, -50244> velocity=< 3,  5>\nposition=<-40148, -19999> velocity=< 4,  2>\nposition=< 20325,  -9922> velocity=<-2,  1>\nposition=< 20305,  30411> velocity=<-2, -3>\nposition=<-20020,  30409> velocity=< 2, -3>\nposition=< -9931,  50566> velocity=< 1, -5>\nposition=< 50582,  50564> velocity=<-5, -5>\nposition=<-50259, -30084> velocity=< 5,  3>\nposition=<-19985,  50564> velocity=< 2, -5>\nposition=< 10256, -19999> velocity=<-1,  2>\nposition=< -9918, -19995> velocity=< 1,  2>\nposition=< 20349,  20330> velocity=<-2, -2>\nposition=< 50592, -50246> velocity=<-5,  5>\nposition=<-30093,  40485> velocity=< 3, -4>\nposition=< -9894,  50565> velocity=< 1, -5>\nposition=< 30418, -19999> velocity=<-3,  2>\nposition=<-30065,  10249> velocity=< 3, -1>\nposition=<-40123,  40492> velocity=< 4, -4>\nposition=<-40150,  40489> velocity=< 4, -4>\nposition=<-40182,  20329> velocity=< 4, -2>\nposition=<-19977,  -9918> velocity=< 2,  1>\nposition=< 30388,  30411> velocity=<-3, -3>\nposition=< 30393,  10245> velocity=<-3, -1>\nposition=< 40471,  -9914> velocity=<-4,  1>\nposition=<-40122,  10240> velocity=< 4, -1>\nposition=< 40524, -40156> velocity=<-4,  4>\nposition=< -9923,  40484> velocity=< 1, -4>\nposition=<-30073,  40491> velocity=< 3, -4>\nposition=< 50590,  40483> velocity=<-5, -4>\nposition=< 10255, -40159> velocity=<-1,  4>\nposition=< 10241, -30079> velocity=<-1,  3>\nposition=< 20364, -40156> velocity=<-2,  4>\nposition=<-40133,  -9922> velocity=< 4,  1>\nposition=< -9897,  40483> velocity=< 1, -4>\nposition=<-50247,  30411> velocity=< 5, -3>\nposition=< 10227,  -9922> velocity=<-1,  1>\nposition=< 30421, -30080> velocity=<-3,  3>\nposition=<-50239,  10244> velocity=< 5, -1>\nposition=<-30077, -19998> velocity=< 3,  2>\nposition=<-19994, -19997> velocity=< 2,  2>\nposition=<-30091,  20321> velocity=< 3, -2>\nposition=<-50239,  -9915> velocity=< 5,  1>\nposition=<-50239, -50238> velocity=< 5,  5>\nposition=<-50254,  50568> velocity=< 5, -5>\nposition=< 20312,  30410> velocity=<-2, -3>\nposition=< 10258,  30411> velocity=<-1, -3>\nposition=<-30061,  50573> velocity=< 3, -5>\nposition=< -9928,  10245> velocity=< 1, -1>\nposition=< 10271, -30080> velocity=<-1,  3>\nposition=<-19972,  10247> velocity=< 2, -1>\nposition=< 40493,  50566> velocity=<-4, -5>\nposition=<-30096, -30083> velocity=< 3,  3>\nposition=< 20332,  -9921> velocity=<-2,  1>\nposition=< 20357, -30081> velocity=<-2,  3>\nposition=<-50263,  40486> velocity=< 5, -4>\nposition=< 10267, -50237> velocity=<-1,  5>\nposition=<-50239,  10242> velocity=< 5, -1>\nposition=< 50583,  20321> velocity=<-5, -2>\nposition=< 30425, -30076> velocity=<-3,  3>\nposition=<-50227,  30411> velocity=< 5, -3>\nposition=<-40125,  20330> velocity=< 4, -2>\nposition=<-20020, -40163> velocity=< 2,  4>\nposition=< 10268, -40163> velocity=<-1,  4>\nposition=<-40181,  20321> velocity=< 4, -2>\nposition=< 10231,  20330> velocity=<-1, -2>\nposition=<-30101,  20322> velocity=< 3, -2>\nposition=<-40157,  20326> velocity=< 4, -2>\nposition=< 30394,  10244> velocity=<-3, -1>\nposition=<-40125,  50564> velocity=< 4, -5>\nposition=< 40498,  20323> velocity=<-4, -2>\nposition=< 50563,  10248> velocity=<-5, -1>\nposition=<-30080,  50573> velocity=< 3, -5>\nposition=< -9896,  20321> velocity=< 1, -2>\nposition=<-50236,  50566> velocity=< 5, -5>\nposition=<-40142, -19994> velocity=< 4,  2>\nposition=< -9939,  -9916> velocity=< 1,  1>\nposition=<-40155,  -9915> velocity=< 4,  1>\nposition=< 20362,  20326> velocity=<-2, -2>\nposition=< 20347,  20325> velocity=<-2, -2>\nposition=< 30397, -19999> velocity=<-3,  2>\nposition=< 10235,  20327> velocity=<-1, -2>\nposition=<-50207, -30075> velocity=< 5,  3>\nposition=< 20333, -20003> velocity=<-2,  2>\nposition=< 40499,  40483> velocity=<-4, -4>\nposition=< 30385,  -9921> velocity=<-3,  1>\nposition=<-50210,  10242> velocity=< 5, -1>\nposition=<-40148,  50564> velocity=< 4, -5>\nposition=< 50582,  40483> velocity=<-5, -4>\nposition=<-30060, -30078> velocity=< 3,  3>\nposition=< 10271,  30406> velocity=<-1, -3>\nposition=< 50599, -40165> velocity=<-5,  4>\nposition=< 30401,  50565> velocity=<-3, -5>\nposition=< 20333, -50237> velocity=<-2,  5>\nposition=<-30053,  10249> velocity=< 3, -1>\nposition=< 20308, -50237> velocity=<-2,  5>\nposition=<-40163, -40161> velocity=< 4,  4>\nposition=< 40474, -50238> velocity=<-4,  5>\nposition=<-30053,  20321> velocity=< 3, -2>\nposition=<-30082, -50241> velocity=< 3,  5>\nposition=< 30429, -50246> velocity=<-3,  5>\nposition=< 30426,  20321> velocity=<-3, -2>\nposition=<-50250, -40157> velocity=< 5,  4>\nposition=< 40483, -50244> velocity=<-4,  5>\nposition=< 20320, -20003> velocity=<-2,  2>\nposition=< 40479, -40156> velocity=<-4,  4>\nposition=< 50568, -50238> velocity=<-5,  5>\nposition=< 50591, -30081> velocity=<-5,  3>\nposition=< 40526,  10240> velocity=<-4, -1>\nposition=< 50595,  20329> velocity=<-5, -2>\nposition=< 50592, -30075> velocity=<-5,  3>\nposition=<-30089,  50571> velocity=< 3, -5>\nposition=<-40169,  20330> velocity=< 4, -2>\nposition=<-20011,  30402> velocity=< 2, -3>\nposition=< 10265,  20330> velocity=<-1, -2>\nposition=<-19988, -20001> velocity=< 2,  2>\nposition=< 30397, -30077> velocity=<-3,  3>\nposition=< 50595,  30407> velocity=<-5, -3>\nposition=<-19972,  -9920> velocity=< 2,  1>\nposition=<-19978, -30084> velocity=< 2,  3>\nposition=< 30401,  10240> velocity=<-3, -1>\nposition=<-30042,  20325> velocity=< 3, -2>\nposition=< 10267,  40483> velocity=<-1, -4>\nposition=< -9939, -20001> velocity=< 1,  2>\nposition=< -9922, -30081> velocity=< 1,  3>\nposition=< 30393,  20321> velocity=<-3, -2>\nposition=< 10235, -30077> velocity=<-1,  3>\nposition=< 50607,  50564> velocity=<-5, -5>\nposition=<-40162, -20001> velocity=< 4,  2>\nposition=< 10271,  20326> velocity=<-1, -2>\nposition=< -9881, -40160> velocity=< 1,  4>\nposition=< 10247,  10246> velocity=<-1, -1>\nposition=<-20020, -30079> velocity=< 2,  3>\nposition=<-40123, -19994> velocity=< 4,  2>\nposition=< 30442, -40159> velocity=<-3,  4>\nposition=< 40483,  10246> velocity=<-4, -1>\nposition=< -9914, -30080> velocity=< 1,  3>\nposition=< 50572, -30079> velocity=<-5,  3>\nposition=< 30435, -30084> velocity=<-3,  3>\nposition=<-19995, -19999> velocity=< 2,  2>\nposition=<-30077,  -9916> velocity=< 3,  1>\nposition=<-19988,  40484> velocity=< 2, -4>\nposition=< 50555,  30403> velocity=<-5, -3>\nposition=< 50595,  30402> velocity=<-5, -3>\nposition=< -9878,  50564> velocity=< 1, -5>\nposition=< -9922, -50244> velocity=< 1,  5>\nposition=<-40141,  30402> velocity=< 4, -3>\nposition=< 30398, -50244> velocity=<-3,  5>\nposition=< -9904, -19999> velocity=< 1,  2>\nposition=<-50243,  40490> velocity=< 5, -4>\nposition=< 40490,  -9918> velocity=<-4,  1>\nposition=< -9907,  -9914> velocity=< 1,  1>\nposition=<-50242,  10249> velocity=< 5, -1>\nposition=<-40171,  20326> velocity=< 4, -2>\nposition=< 10247, -40164> velocity=<-1,  4>\nposition=<-30098, -30084> velocity=< 3,  3>\nposition=<-40155,  10242> velocity=< 4, -1>\nposition=< -9895,  10249> velocity=< 1, -1>\nposition=< 30442,  50573> velocity=<-3, -5>\nposition=<-19967,  50565> velocity=< 2, -5>\nposition=< 50559,  20321> velocity=<-5, -2>\nposition=< -9919, -30081> velocity=< 1,  3>\nposition=< -9905,  30406> velocity=< 1, -3>\nposition=< 50572,  -9917> velocity=<-5,  1>\nposition=< 10255,  10248> velocity=<-1, -1>\nposition=< -9938,  -9922> velocity=< 1,  1>\nposition=< 40483,  40486> velocity=<-4, -4>\nposition=<-50242,  40484> velocity=< 5, -4>\nposition=< 20352, -30083> velocity=<-2,  3>\nposition=< 30446,  40492> velocity=<-3, -4>\nposition=< -9936, -50237> velocity=< 1,  5>\nposition=< 50568, -20002> velocity=<-5,  2>\nposition=<-20016,  20330> velocity=< 2, -2>\nposition=< 50596, -20003> velocity=<-5,  2>\nposition=< 30425,  10240> velocity=<-3, -1>\nposition=< 50567, -30077> velocity=<-5,  3>\nposition=< 50606,  20325> velocity=<-5, -2>\nposition=< 50571, -20000> velocity=<-5,  2>\nposition=< 20337,  40492> velocity=<-2, -4>\nposition=< 40478,  -9918> velocity=<-4,  1>\nposition=<-30069,  20321> velocity=< 3, -2>\nposition=< 30401, -19994> velocity=<-3,  2>\nposition=< 50583, -50237> velocity=<-5,  5>\nposition=< 20317, -20002> velocity=<-2,  2>\nposition=< 10231,  -9915> velocity=<-1,  1>\nposition=< 20328,  30409> velocity=<-2, -3>\nposition=< 10233,  10244> velocity=<-1, -1>\nposition=<-19972,  50565> velocity=< 2, -5>\nposition=<-40134,  40491> velocity=< 4, -4>\nposition=< 40466,  50568> velocity=<-4, -5>\nposition=<-50215, -30079> velocity=< 5,  3>\nposition=< 10259,  20325> velocity=<-1, -2>\nposition=< -9939,  40484> velocity=< 1, -4>\nposition=<-50231,  50568> velocity=< 5, -5>\nposition=< 40467,  40492> velocity=<-4, -4>\nposition=< 10255, -30083> velocity=<-1,  3>\nposition=<-50222,  -9916> velocity=< 5,  1>\nposition=< -9927,  30408> velocity=< 1, -3>\nposition=< 20304,  20329> velocity=<-2, -2>\nposition=<-30057,  10249> velocity=< 3, -1>\nposition=< 20312,  20323> velocity=<-2, -2>\nposition=<-40182,  30405> velocity=< 4, -3>\nposition=<-30050, -40161> velocity=< 3,  4>\nposition=< 20317,  10243> velocity=<-2, -1>\nposition=<-50250,  -9919> velocity=< 5,  1>\nposition=<-30093,  50568> velocity=< 3, -5>\nposition=< 50555, -30080> velocity=<-5,  3>\nposition=< 50571,  -9915> velocity=<-5,  1>\nposition=< 40468,  30411> velocity=<-4, -3>\nposition=<-40180, -50237> velocity=< 4,  5>\nposition=< 10264,  -9913> velocity=<-1,  1>\nposition=< 50560, -50243> velocity=<-5,  5>\nposition=<-40170, -50242> velocity=< 4,  5>\nposition=< 20347,  10240> velocity=<-2, -1>\nposition=<-20018, -20003> velocity=< 2,  2>\nposition=< -9878,  20322> velocity=< 1, -2>\nposition=<-30069,  40486> velocity=< 3, -4>\nposition=<-50214,  10244> velocity=< 5, -1>\nposition=< 30427,  30407> velocity=<-3, -3>\nposition=< -9918,  -9922> velocity=< 1,  1>\nposition=< 40469,  10249> velocity=<-4, -1>\nposition=< 50575,  50572> velocity=<-5, -5>\nposition=< 50567,  50570> velocity=<-5, -5>\nposition=< 30427,  -9917> velocity=<-3,  1>\nposition=<-40162,  40485> velocity=< 4, -4>\nposition=<-19988,  40488> velocity=< 2, -4>\nposition=< -9890,  50568> velocity=< 1, -5>\nposition=<-30098,  20321> velocity=< 3, -2>\nposition=<-50207,  40483> velocity=< 5, -4>\nposition=< 40517,  50568> velocity=<-4, -5>\nposition=< 10275,  30402> velocity=<-1, -3>\nposition=< 50588,  10246> velocity=<-5, -1>\nposition=< -9883,  -9914> velocity=< 1,  1>\nposition=<-40166, -50238> velocity=< 4,  5>\nposition=< 10255, -40156> velocity=<-1,  4>\nposition=< 40524,  -9913> velocity=<-4,  1>\nposition=< 50551,  30402> velocity=<-5, -3>\nposition=<-30077, -20003> velocity=< 3,  2>\nposition=<-40182, -50243> velocity=< 4,  5>\nposition=<-30053, -19994> velocity=< 3,  2>\nposition=< 40506, -30084> velocity=<-4,  3>\nposition=<-50244,  -9918> velocity=< 5,  1>\nposition=< 30395,  20321> velocity=<-3, -2>\nposition=< 20338, -40156> velocity=<-2,  4>\nposition=<-30057,  50567> velocity=< 3, -5>\nposition=<-30100, -30084> velocity=< 3,  3>\nposition=<-20003,  50571> velocity=< 2, -5>\nposition=< 20328, -30075> velocity=<-2,  3>\nposition=< 20317,  -9921> velocity=<-2,  1>\nposition=< 20336, -40157> velocity=<-2,  4>\nposition=<-19988,  10246> velocity=< 2, -1>\nposition=<-40158,  50565> velocity=< 4, -5>\nposition=< 40506,  30410> velocity=<-4, -3>\nposition=<-19971,  -9918> velocity=< 2,  1>\nposition=< 50571, -19995> velocity=<-5,  2>\nposition=<-40131,  10240> velocity=< 4, -1>\nposition=<-30099, -19994> velocity=< 3,  2>\nposition=< 10223,  30409> velocity=<-1, -3>\nposition=< 20364, -50243> velocity=<-2,  5>\nposition=< 30393, -40162> velocity=<-3,  4>\nposition=< 40526,  40492> velocity=<-4, -4>\nposition=<-50239,  -9917> velocity=< 5,  1>\nposition=< -9907, -30077> velocity=< 1,  3>\nposition=< -9939,  40491> velocity=< 1, -4>\nposition=<-19964,  30410> velocity=< 2, -3>\nposition=< 50580, -19994> velocity=<-5,  2>\nposition=<-40164,  50568> velocity=< 4, -5>\nposition=<-30053, -40165> velocity=< 3,  4>\nposition=< 10240, -40158> velocity=<-1,  4>\nposition=< -9923,  50564> velocity=< 1, -5>\nposition=< 50605,  50564> velocity=<-5, -5>\nposition=< 30393, -40159> velocity=<-3,  4>\nposition=< -9939,  50568> velocity=< 1, -5>\nposition=<-40150,  50573> velocity=< 4, -5>\nposition=< 50566, -40161> velocity=<-5,  4>\nposition=<-19975,  -9913> velocity=< 2,  1>\nposition=<-40162,  40489> velocity=< 4, -4>\nposition=< -9913, -30081> velocity=< 1,  3>\nposition=< 10271, -50240> velocity=<-1,  5>\nposition=<-40164, -30079> velocity=< 4,  3>\nposition=< -9883, -40158> velocity=< 1,  4>\nposition=< 20355, -30084> velocity=<-2,  3>\nposition=< 40510,  -9922> velocity=<-4,  1>\nposition=<-30090,  20325> velocity=< 3, -2>\nposition=<-20003, -19996> velocity=< 2,  2>\nposition=<-30085,  20322> velocity=< 3, -2>\nposition=<-30099, -20003> velocity=< 3,  2>\nposition=< -9880,  -9922> velocity=< 1,  1>\nposition=< 50607, -19994> velocity=<-5,  2>\nposition=<-50207,  30402> velocity=< 5, -3>"
