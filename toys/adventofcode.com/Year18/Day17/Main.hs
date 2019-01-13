{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/17

module Main where

import           Data.Maybe
import           Data.Set        (Set, (\\))
import qualified Data.Set        as S
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.List       (foldl', intercalate)
import           Util.Main1 (main12)

import           Control.Arrow (first, second, (***))
import           Control.Monad.State.Strict
import           Control.Monad.Except

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char

import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  inp <- inp'
  main12 "2018-15"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

withTrace :: Bool
withTrace = True -- False

trace' :: String -> a -> a
trace' s
  | withTrace = trace s
  | otherwise = id

-- ----------------------------------------

type Point          = (Int, Int)
type Ps             = Set Point
type Rect           = (Point, Point)

data Board          = B { clay    :: Ps
                        , filled  :: Ps
                        , flow    :: Ps
                        , geo     :: Rect
                        , org     :: Rect
                        }
                    deriving (Show)

-- --------------------

emptyBoard :: Board
emptyBoard = B { clay   = emptyPs
               , filled = emptyPs
               , flow   = emptyPs
               , geo    = ((0, 0),(0,0))
               , org    = ((0, 0),(0,0))
               }

emptyPs :: Ps
emptyPs = S.empty

singlePs :: Point -> Ps
singlePs = S.singleton

u :: Ps -> Ps -> Ps
u = S.union

n :: Ps -> Ps -> Ps
n = S.intersection

e :: Point -> Ps -> Bool
e = S.member

nullPs :: Ps -> Bool
nullPs = S.null

move' :: Rect -> (Point -> Point) -> Ps -> Ps
move' ((_x0, y0), (_x1, y1)) f ps =
  foldl' ins emptyPs ps
  where
    ins res p
      | y0 <= y
        &&
        y <= y1   = S.insert p' res
      | otherwise = res              -- out of range
      where
        p'@(_x, y) = f p

up' :: Rect -> Ps -> Ps
up' = flip move' (second pred)

down' :: Rect -> Ps -> Ps
down' = flip move' (second succ)

left' :: Rect -> Ps -> Ps
left' = flip move' (first pred)

right' :: Rect -> Ps -> Ps
right' = flip move' (first succ)

northEast' :: Rect -> Ps -> Ps
northEast' = flip move' (succ *** pred)

northWest' :: Rect -> Ps -> Ps
northWest' = flip move' (pred *** pred)

liftMove :: (Rect -> Ps -> Ps) -> (Ps -> Action Ps)
liftMove mf ps = do
  g <- getGeo
  return (mf g ps)

up
  , down
  , left
  , right
  , northEast
  , northWest :: Ps -> Action Ps
up        = liftMove up'
down      = liftMove down'
left      = liftMove left'
right     = liftMove right'
northEast = liftMove northEast'
northWest = liftMove northWest'

-- ----------------------------------------

type Action     = ExceptT GameTerminated (State Board)

data GameTerminated = AllFilled deriving (Eq, Show)

runAction :: Action a -> Board -> (Either GameTerminated a, Board)
runAction action =
  runState (runExceptT action)

execAction :: Action a -> Board -> Board
execAction action = snd . runAction action

getClay :: Action Ps
getClay = gets clay

getWater :: Action (Ps, Ps)
getWater = do
  s <- get
  return (filled s, flow s)

getFlow :: Action Ps
getFlow = gets flow

getAll :: Action (Ps, Ps, Ps)
getAll = do
  s <- get
  return (clay s, filled s, flow s)

getBlocked :: Action Ps
getBlocked = do
  s <- get
  return (clay s `u` filled s)

getNoSand :: Action Ps
getNoSand = do
  s <- get
  return (clay s `u` filled s `u` flow s)

getGeo :: Action Rect
getGeo = gets geo

filterFlow :: Ps -> Action Ps
filterFlow s = do
  fl <- getFlow
  return $ s `n` fl

filterClay :: Ps -> Action Ps
filterClay s = do
  cl <- getClay
  return $ s `n` cl

filterBlocked :: Ps -> Action Ps
filterBlocked s = do
  bl <- getBlocked
  return $ s `n` bl

filterSand :: Ps -> Action Ps
filterSand s = do
  ns <- getNoSand
  return $ s \\ ns

modifyFlow :: (Ps -> Ps) -> Action ()
modifyFlow mf =
  modify (\ s -> s { flow = mf (flow s)})

modifyFilled :: (Ps -> Ps) -> Action ()
modifyFilled mf =
  modify (\ s -> s { filled = mf (filled s)})

trcBoard :: Action ()
trcBoard = do
  a <- getAll
  g <- getGeo
  trace' (showBoard g a) $ return ()

trcWater :: Action ()
trcWater = do
  (fi, fl) <- (S.size *** S.size) <$> getWater
  trace' ("(filled, flow) = " ++ show (fi, fl) ++ " = " ++ show (fi + fl)) $ return ()

-- ----------------------------------------

letItFlow :: Action ()
letItFlow = getFlow >>= letItFlow'

letItFlow' :: Ps -> Action ()
letItFlow' f = do
  fd <- down f
  fs <- filterSand fd
  f' <- filterBlocked fd >>= up
  fl <- left  f' >>= filterSand
  fr <- right f' >>= filterSand
  let res = fs `u` fl `u` fr
  modifyFlow (res `u`)
  unless (nullPs res) $
    letItFlow' res

checkFilledLeft :: Ps -> Action Ps
checkFilledLeft = left >=> filterClay    >=> right

checkFilledDown :: Ps -> Action Ps
checkFilledDown = down >=> filterBlocked >=> up


flowToFill :: Action ()
flowToFill = do
  f  <- getFlow >>= checkFilledLeft >>= checkFilledDown

  f' <- foldl' S.union S.empty <$>     -- scan these flow's for conversion into filled
        (mapM areFilled $ map singlePs $ S.toList f)

  modifyFlow   (\\  f')                -- update flow and filled
  modifyFilled (`u` f')

  where
    areFilled :: Ps -> Action Ps
    areFilled ps = do
      ps1 <- right ps
      scanRight ps1 ps

    scanRight :: Ps -> Ps -> Action Ps
    scanRight ps acc = do
      isClay <- (not . nullPs) <$>  filterClay ps
      isFill <- (not . nullPs) <$> (filterFlow ps >>= checkFilledDown)
      if isClay
        then           -- right # found
          return acc
        else
          if isFill
          then do
            ps1 <- right ps
            scanRight ps1 (acc `u` ps)
          else
            return emptyPs

flowAndFill :: Action ()
flowAndFill =
  letItFlow  >> trcWater >> -- trcBoard >>
  flowToFill >> trcWater -- >> trcBoard

overflow :: Action ()
overflow = do
  w0 <- getWater
  flowAndFill
  w1 <- getWater
  when (w0 /= w1)
    overflow

-- ----------------------------------------


solve2 :: Input -> Int
solve2 = cntWater . snd . runAction overflow . inp2Board
  where
    cntWater b = S.size . move' (org b) id $ filled b

solve1 :: Input -> Int
solve1 = cntWater . snd . runAction overflow . inp2Board
  where
    cntWater b = S.size . move' (org b) id $ filled b `u` flow b

solve' act xs = sb . snd . run . inp2Board $ xs
  where
    -- sb (B cl fi fl g og) = show (S.size fi + S.size fl - 1)
    -- sb (B cl fi fl g og) = show (g, og)
    sb (B cl fi fl g og) = S.size . move' og id $ fi `u` fl
    run s = runAction act s

inp2Board :: Input -> Board
inp2Board inp =
  B { clay   = cs
    , filled = emptyPs
    , flow   = singlePs (500,0)
    , geo    = (minP, maxP)
    , org    = (minO, maxO)
    }
  where
    cs   = foldl' ins emptyPs inp
      where
        ins m ((minX, maxX), (minY, maxY)) =
          S.union m $ S.fromList m2
          where
            m2 =[(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]

    minP = ( iX - 1
           , iY `min` 0
           )

    maxP = ( aX + 1
           , aY
           )

    minO@(iX, iY)
         = ( (minimum . map (fst . fst) $ inp)
           , (minimum . map (fst . snd) $ inp)
           )

    maxO@(aX, aY)
         = ( (maximum . map (snd . fst) $ inp)
           ,  maximum . map (snd . snd) $ inp
           )

-- ----------------------------------------

showBoard :: Rect -> (Ps, Ps, Ps) -> String
showBoard ((minX, minY), (maxX, maxY)) (cl, fi, fl) =
  unlines showBoard'
  where
    showBoard' :: [String]
    showBoard' = map toLine [minY..maxY]
      where
        toLine y = map toC [minX..maxX]
          where
            toC x
              | p `e` cl  = '#'
              | p `e` fi  = '~'
              | p `e` fl  = if x == 500
                               &&
                               y == 0
                            then '+'
                            else '|'
              | otherwise = '.'
              where
                p = (x, y)

-- ----------------------------------------

type Input = [Rect]

fromString :: String -> Input
fromString = map parseLine . lines

parseLine :: String -> Rect
parseLine line = maybe (error "no parse") id $ parseMaybe pPoints2 line

type SP    = Parsec () String
type FctP  = Point -> Point
type FctPP = Rect -> Rect

pPoints2 :: SP Rect
pPoints2 = do
  p1 <- pPoint <* string ", "
  p2 <- pPoint
  return (p2 . p1 $ (undefined, undefined))

pPoint :: SP FctPP
pPoint =
  pPoint' 'x' (\ p -> first (const p))
  <|>
  pPoint' 'y' (\ p -> second (const p))

pPoint' :: Char -> (Point -> FctPP) -> SP FctPP
pPoint' c f = do
  p <- char c *> string "=" *> pPt
  return (f p)

pPt :: SP Point
pPt = do
  lb <- num
  ub <- option lb $ string ".." *> num
  return (lb, ub)
  where
    num :: SP Int
    num = read <$> some digitChar

-- ----------------------------------------

ex :: String
ex = unlines $
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504"
  ]

ex2 :: String
ex2 = unlines $
  [ "x=495, y=1..4"
  , "x=505, y=3..4"
  , "x=509, y=1..3"
  , "y=5, x=495..505"
  , "x=507, y=2..7"
  ]

inp' :: IO String
inp' = readFile "Year18/Day17/day17.txt"

-- inp :: String
-- inp = ""

-- ----------------------------------------
