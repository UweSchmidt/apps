{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/15

module Main where

import           Data.Maybe
import           Data.Set        (Set, (\\))
import qualified Data.Set        as S
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.List       (foldl')
import           Util.Main1 (main12)

import           Control.Arrow (first, second, (***))
import           Control.Monad.State.Strict
import           Control.Monad.Except

import Debug.Trace

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

type Point          = (Int, Int)
type Ps             = Set Point
type HitPointMap    = Map Point HitPoints
type HitPoints      = Int

data Board          = B { walls   :: Ps
                        , elves   :: Ps
                        , goblins :: Ps
                        , rounds  :: Int
                        , hits    :: HitPointMap
                        }
                    deriving (Show)

-- --------------------

emptyBoard :: Board
emptyBoard = B { walls   = emptyPs
               , elves   = emptyPs
               , goblins = emptyPs
               , hits    = emptyHitPointMap
               , rounds  = 0
               }

emptyPs :: Ps
emptyPs = S.empty

singlePs :: Point -> Ps
singlePs = S.singleton

emptyHitPointMap :: HitPointMap
emptyHitPointMap = M.empty

initHitPoints :: HitPoints
initHitPoints = 200

attacPoints :: Int
attacPoints = 3

u :: Ps -> Ps -> Ps
u = S.union

n :: Ps -> Ps -> Ps
n = S.intersection

e :: Point -> Ps -> Bool
e = S.member

nullPs :: Ps -> Bool
nullPs = S.null

mvp :: Point -> Point -> Ps -> Ps
mvp p0 p1 ps
  | p0 `e` ps = S.insert p1 . S.delete p0 $ ps
  | otherwise        =                             ps

neighbors1 :: Point -> Ps
neighbors1 (y, x) = S.fromList
                    [ (y + 1, x    )
                    , (y - 1, x    )
                    , (y    , x + 1)
                    , (y    , x - 1)
                    ]

neighbors :: Ps -> Ps
neighbors = S.foldl' (\ ps' p -> ps' `u` neighbors1 p) emptyPs

-- ----------------------------------------

type Action     = ExceptT String (State Board)

runAction :: Action a -> Board -> (Either String a, Board)
runAction action board0 =
  runState (runExceptT action) board0

incrRounds :: Action ()
incrRounds = modify (\ s -> s { rounds = rounds s + 1})

getWalls :: Action Ps
getWalls = gets walls

getElves :: Action Ps
getElves = gets elves

getGoblins :: Action Ps
getGoblins = gets goblins

getEGs :: Action (Ps, Ps)
getEGs = gets (\ s -> (elves s, goblins s))

moveElf :: Point -> Point -> Action ()
moveElf p0 p1 =
  modify (\ s -> s { elves = mvp p0 p1 $ elves s})

moveGoblin :: Point -> Point -> Action ()
moveGoblin p0 p1 =
  modify (\ s -> s { goblins = mvp p0 p1 $ goblins s})

getUnits :: Action Ps
getUnits = gets (\ s -> elves s `u` goblins s)

getClosed :: Action Ps
getClosed = gets (\ s -> walls s `u` elves s `u` goblins s)

getHitPointMap :: Action HitPointMap
getHitPointMap = gets hits

setHitPointMap :: HitPointMap -> Action ()
setHitPointMap hpm = modify (\ s -> s {hits = hpm})

moveHitPoint :: Point -> Point -> Action ()
moveHitPoint p0 p1 = do
  mhp <- getHitPoints p0
  case mhp of
    Just hp -> modify (\ s -> s { hits = M.insert p1 hp . M.delete p0 $ hits s})
    Nothing -> return ()

getHitPoints :: Point -> Action (Maybe HitPoints)
getHitPoints p = gets (\ s -> M.lookup p $ hits s)

decrHitPoints :: Point -> Int -> Action ()
decrHitPoints p d = modify (\ s -> s { hits = M.adjust (\ x -> x - d) p (hits s)})

trcBoard :: Action ()
trcBoard = return ()
{-
trcBoard = do
  ws <- getWalls
  (es, gs) <- getEGs
  trace (showBoard (ws, es, gs)) $ return ()
-}
trcRound :: Action () -> Action ()
trcRound act = do
  i <- gets rounds
  trace ("start round " ++ show i) (return ())
    >> act
    >> do
       h <- getHitPointMap
       trace ("finished round " ++ show i ++ "\nhitpoints " ++ show h) (return ())

-- ----------------------------------------

initHitPointMap :: HitPoints -> Action ()
initHitPointMap hp = do
  us <- getUnits
  setHitPointMap $ S.foldl' (\ m p -> M.insert p hp m) emptyHitPointMap us

moveUnit :: Point -> Point -> Action ()
moveUnit p0 p1 = do
  moveElf      p0 p1
  moveGoblin   p0 p1
  moveHitPoint p0 p1

removeUnit :: Point -> Action ()
removeUnit p0 =
  modify (\ s -> s { elves   = S.delete p0 $ elves s
                   , goblins = S.delete p0 $ goblins s
                   , hits    = M.delete p0 $ hits s
                   }
         )

enemiesInRange :: Point -> Action Ps
enemiesInRange p = do
  let ns   =  neighbors1 p
  (es, gs) <- getEGs
  let rs | p `e` es = gs `n` ns
         | p `e` gs = es `n` ns
         | otherwise = emptyPs
  return rs

enemyInRange :: Point -> Action (Maybe Point)
enemyInRange p = do
  rs <- enemiesInRange p
  hm <- getHitPointMap
  return $ minHps hm rs
  where
    minHps :: HitPointMap -> Ps -> Maybe Point
    minHps hm rs = fmap snd . S.lookupMin . S.map (\ p -> (hm M.! p, p)) $ rs

-- if enemy not in range,
-- move a unit a step towards the nearest enemy
moveToEnemy :: Point -> Action (Maybe Point)
moveToEnemy p = do
  ps1      <- (neighbors1 p \\) <$> getWalls
  (es, gs) <- getEGs
  let (es', gs')
        | p `e` es  = (es, gs)
        | p `e` gs  = (gs, es)
        | otherwise = (emptyPs, emptyPs)
  S.lookupMin <$> searchEnemy es' gs' (ps1 \\ es') (singlePs p)

{-
searchEnemy :: Ps -> Ps -> Ps -> Ps -> Action Ps
searchEnemy buddies enemies border visited =
  trace ("searchEnemy") $
  trace (show buddies) $
  trace (show enemies) $
  trace (show border)  $
  trace (show visited) $ do
  res <- searchEnemy' buddies enemies border visited
  trace ("searchEnemy res=" ++ show res) $ return res
-}
searchEnemy :: Ps -> Ps -> Ps -> Ps -> Action Ps
searchEnemy buddies enemies border visited
  | not (nullPs enemies') =
      return (singlePs . S.findMin $ enemies') -- the nearest enemy found
  | otherwise = do
      walls' <- getWalls
      let border' = neighbors border \\ buddies \\ visited' \\ walls'
      if nullPs border'                             -- no more open places
        then
          return border'
        else do                                -- next search step
          rs1 <- searchEnemy buddies enemies border' visited'
          return $ neighbors rs1 `n` border    -- compute the way(s) back

  where
    enemies' = border `n` enemies
    visited' = visited `u` border

allRounds :: Action ()
allRounds = do
  end <- oneHasDied
  when (not end) $ do
    trcRound singleRound
    allRounds


oneHasDied :: Action Bool
oneHasDied =
  (\ (es, gs) -> nullPs es || nullPs gs) <$> getEGs

singleRound :: Action ()
singleRound = do
  incrRounds
  ps <- getUnits
  mapM_ (\ p -> processUnit p >> trcBoard) ps

processUnit :: Point -> Action ()
processUnit p = do
  trace ("processUnit p=" ++ show p) $ return ()

  mp <- tryMoveUnit p
  case mp of
    Just p2 -> tryAttacUnit p2
    Nothing -> tryAttacUnit p

tryMoveUnit :: Point -> Action (Maybe Point)
tryMoveUnit p = do
  mp <- enemyInRange p
  case mp of
    Nothing -> do
      mp2 <- moveToEnemy p
      case mp2 of
        Just e2 -> moveUnit p e2 >> return mp2
        Nothing -> return Nothing
    Just _ -> return Nothing

tryAttacUnit :: Point -> Action ()
tryAttacUnit p = do
  mp <- enemyInRange p
  case mp of
    Just e1 -> do                    -- attac
      decrHitPoints e1 attacPoints
      hp <- fromMaybe 1 <$> getHitPoints e1
      when (hp <= 0) $
        removeUnit e1

    Nothing -> return ()

-- ----------------------------------------

solve1 :: (Ps, Ps, Ps) -> Board
solve1 (ws, es, gs) = snd . runAction (forever singleRound) $ b0
  where
    b0 = emptyBoard
         { walls   = ws
         , elves   = es
         , goblins = gs
         }

solve2 :: (Ps, Ps, Ps) -> Board
solve2 (ws, es, gs) = snd . runAction (return ()) $ b0
  where
    b0 = emptyBoard
         { walls   = ws
         , elves   = es
         , goblins = gs
         }

solve' :: (Ps, Ps, Ps) -> Int
solve' (ws, es, gs) =
  trace (showBoard (walls s, elves s, goblins s)) $
  trace (show $ hits s) $
  trace (show $ rounds s)
  (rounds s -1) * (sum . M.elems $ hits s)
  where
    s  = snd . runAction act $ b0

    b0 = emptyBoard
         { walls   = ws
         , elves   = es
         , goblins = gs
         }
    act = initHitPointMap 200 >> allRounds

-- ----------------------------------------

showBoard :: (Ps, Ps, Ps) -> String
showBoard (ws, es, gs) =
  unlines $ map toLine [0..maxY]
  where
    toLine y = map toC [0..maxX]
      where
        toC x
          | p `e` ws  = '#'
          | p `e` es  = 'E'
          | p `e` gs  = 'G'
          | otherwise = '.'
          where
            p = (y, x)

    as = ws `u` es `u` gs
    (maxY, maxX) = (maximum *** maximum ) . unzip $ S.elems as

toString :: Board -> String
toString = show -- x ++ "," ++ show y

fromString :: String -> (Ps, Ps, Ps)
fromString = parseLines . lines

parseLines :: [String] -> (Ps, Ps, Ps)
parseLines =
  merges . zipWith parseLine [0..]
  where
    parseLine y =
      merges . zipWith parseChar [0..]
      where
        parseChar :: Int -> Char -> (Ps, Ps, Ps)
        parseChar x c = case c of
          '#' -> (singlePs p, emptyPs, emptyPs)
          'E' -> (emptyPs, singlePs p, emptyPs)
          'G' -> (emptyPs, emptyPs, singlePs p)
          _   -> emptyPs3
          where
            p = (y, x)

emptyPs3 :: (Ps, Ps, Ps)
emptyPs3 = (emptyPs, emptyPs, emptyPs)

merge :: (Ps, Ps, Ps) -> (Ps, Ps, Ps) -> (Ps, Ps, Ps)
merge (ws1, es1, gs1) (ws2, es2, gs2) =
  (ws1 `u` ws2, es1 `u` es2, gs1 `u` gs2)

merges :: [(Ps, Ps, Ps)] -> (Ps, Ps, Ps)
merges = foldl' merge emptyPs3

ex :: String
ex = unlines
  [ "#########"
  , "#G..G..G#"
  , "#.......#"
  , "#.......#"
  , "#G..E..G#"
  , "#.......#"
  , "#.......#"
  , "#G..G..G#"
  , "#########"
  ]

ex2 :: String
ex2 = unlines
  [ "#######"
  , "#.G...#"
  , "#...EG#"
  , "#.#.#G#"
  , "#..G#E#"
  , "#.....#"
  , "#######"
  ]

ex3 :: String
ex3 = unlines
  [ "#######"
  , "#G..#E#"
  , "#E#E.E#"
  , "#G.##.#"
  , "#...#E#"
  , "#...E.#"
  , "#######"
  ]

ex9 :: String
ex9 = unlines
  [ "#######"
  , "#.E...#"
  , "#.#..G#"
  , "#.###.#"
  , "#E#G#G#"
  , "#...#G#"
  , "#######"
  ]

ex10 :: String
ex10 = unlines
  [ "#########"
  , "#G......#"
  , "#.E.#...#"
  , "#..##..G#"
  , "#...##..#"
  , "#...#...#"
  , "#.G...G.#"
  , "#.....G.#"
  , "#########"
  ]
inp' :: IO String
inp' = readFile "Year18/Day15/day15.txt"

inp :: String
inp = "################################\n########.#######################\n#######..#######################\n######..########################\n###....####...##################\n###.#..####G..##################\n###G#.G#####..####G#############\n##....G..###.......#############\n#G#####...#..G.....#############\n#G.###..#..G........############\n#..G.G..........G.....#.G.######\n###......GG..G............######\n#######....G..#####.G...#.######\n#######......#######....########\n#######.....#########..........#\n#######.....#########.........##\n#######...#.#########.........##\n#######.....#########........###\n#######.....#########.........##\n#######....E.#######........#..#\n#######.......#####E........####\n###.#.E..#.....G.........#..####\n###......#E......E..G...E...####\n##...........#.............#####\n#####.###..............E...#####\n#############..............#####\n#############..E.....###...#####\n###############..E...###...#####\n#################.E#.####..#####\n#################..#.###########\n#################..#.###########\n################################\n"
