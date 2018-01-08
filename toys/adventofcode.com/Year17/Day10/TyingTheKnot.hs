-- solution for
-- http://adventofcode.com/2017/day/10

module Year17.Day10.TyingTheKnot
  (tieKnots, knotHash, day10)
where

import Util.Main1    (main12)
import Data.List     (foldl', unfoldr)
import Data.Bits     (xor)
import Text.Printf   (printf)

-- ----------------------------------------

data Ring a =
  Ring { _rot  :: Int
       , _len  :: Int
       , _vals :: [a]
       }
  deriving (Show)

mkRing :: [a] -> Ring a
mkRing xs = Ring 0 (length xs) xs

rotateRing :: Int -> Ring a -> Ring a
rotateRing n0 r@(Ring rot len vals)
  | n == 0    = r
  | otherwise = Ring ((rot + n) `mod` len) len (ys ++ xs)
  where
    n = n0 `mod` len
    (xs, ys) = splitAt n vals

normalRing :: Ring a -> Ring a
normalRing r@(Ring rot len v) =
  rotateRing (len - rot) r

reversePrefix :: Int -> Ring a -> Ring a
reversePrefix n0 r@(Ring rot len val)
  | n <= 1 = r
  | otherwise = Ring rot len $ reverse xs ++ ys
  where
    n = n0 `mod` len
    (xs, ys) = splitAt n val

-- ----------------------------------------
--
-- part 1

tieKnots :: String -> String
tieKnots = show . toRes . shuffle . fromString

toRes :: Ring Int -> Int
toRes = product . take 2 . _vals

shuffle :: [Int] -> Ring Int
shuffle = shuffle' 256

shuffle' :: Int -> [Int] -> Ring Int
shuffle' len =
    normalRing
  . foldl' tieKnot (mkRing [0 .. len-1])
  . zip [0..]
  where
    tieKnot :: Ring Int -> (Int, Int) -> Ring Int
    tieKnot r (step, width) =
      rotateRing (width + step) $ reversePrefix width r

-- ----------------------------------------

fromString :: String -> [Int]
fromString s = read $ "[" ++ s ++ "]"

ex :: Int
ex = toRes . shuffle' 5 $ [3, 4, 1, 5]

-- result of input day11
res :: String
res = tieKnots day10

-- test input from adventofcode
day10 :: String
day10 = "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63"

-- ----------------------------------------

knotHash :: String -> String
knotHash = toRes2 . toDense . _vals . shuffle . prepSeq . fromString2

prepSeq :: [Int] -> [Int]
prepSeq xs =
      concat
    . replicate 64
    $ (xs ++ [17, 31, 73, 47, 23])

toDense :: [Int] -> [Int]
toDense = map xors . part 16

xors :: [Int] -> Int
xors = foldl' xor 0

part :: Int -> [a] -> [[a]]
part n = unfoldr pick
  where
    pick :: [a] -> Maybe ([a], [a])
    pick [] = Nothing
    pick xs = Just $ splitAt n xs

toRes2 :: [Int] -> String
toRes2 = concatMap (printf "%02x")

fromString2 :: String -> [Int]
fromString2 = map fromEnum

-- ----------------------------------------

{-
The empty string becomes a2582a3a0e66e6e86e3812dcb672a272.
AoC 2017 becomes 33efeb34ea91902bb2f59c9920caa6cd.
1,2,3 becomes 3efbe78a8d82f29979031a4aa0b16a9d.
1,2,4 becomes 63960835bcdc130f0b66d7ff4f6a5a8e.
-}

-- ----------------------------------------
