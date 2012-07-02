{-# LANGUAGE BangPatterns #-}

module Main
where

import Control.Monad	( mzero
                        , mplus
                        , (>=>)
                        , liftM
                        , foldM
                        )
import Control.Arrow    ( (&&&)
                        , (***)
                        , second
                        )

import Data.List        ( tails
                        , minimumBy
                        )

import Data.Maybe	( fromMaybe
                        , fromJust
                        )

import qualified Data.Map       as M
import qualified Data.List      as L
import qualified Data.Heap      as H

import System.IO ( hFlush, stdout, stderr, hPutStrLn )

-- ----------------------------------------

-- data type invariants
--
-- to turn off any checks,
-- check can be redefined in the instances
-- as check = id

class Invariant a where
    inv   :: a -> Maybe String
    check :: a -> a
    check = id
{-
    check x = case inv x of
                Nothing -> x
                Just e  -> error e
-- -}
infixr 3 .&&.

(.&&.) :: Maybe a -> Maybe a -> Maybe a
x .&&. y = x `mplus` y

ok :: Maybe String
ok = mzero

bad :: [String] -> Maybe String
bad = return . unwords

invCond :: Bool -> [String] -> Maybe String
invCond b e
    | b         = ok
    | otherwise = bad e

illegal :: Show a => String -> a -> b
illegal f v = error . unwords $
              ["illegal argument in", show f ++ ":", show v]

-- ----------------------------------------

type Filter a b = a -> [b]

orElse :: Filter a b -> Filter a b -> Filter a b
orElse f1 f2
    = \ x -> let r1 =  f1 x in
             if null r1 then f2 x else r1

plus :: Filter a b -> Filter a b -> Filter a b
plus f1 f2
    = \ x -> f1 x ++ f2 x

this :: Filter a a
this = (:[])

none :: Filter a b
none = const []

guards :: (a -> Bool) -> Filter a a -> Filter a a
guards p f
    = \ x -> (if p x then f else none) $ x

-- ----------------------------------------

class Figure a where
    width  :: a -> Width
    width  = const 0

    height :: a -> Width
    height = const 0

    xpos   :: a -> Pos
    xpos _ = 0

    ypos   :: a -> Pos
    ypos _ = 0

    xy     :: a -> Point
    xy f   = PT (xpos f) (ypos f)

    wh     :: a -> (Int, Int)
    wh f   = (width f, height f)

coords :: Figure a => a -> [(Pos, Pos)]
coords f
    = [(x, y) | x <- [x0..x1]
              , y <- [y0..y1]
      ]
    where
      PT x0 y0 = xy f
      x1 = x0 + width  f - 1
      y1 = y0 + height f - 1


-- ----------------------------------------

class Size a where
    size :: a -> Int

-- ----------------------------------------

class Area a where
    area :: a -> Int

-- ----------------------------------------

class Mirror a where
    mirror :: a -> a
  
-- ----------------------------------------

newtype Square = SQ Int
    deriving (Eq, Ord, Show)

instance Figure Square where
    width  (SQ i) = i
    height (SQ i) = i

instance Size Square where
    size (SQ i) = i

instance Area Square where
    area (SQ i) = i * i

instance Mirror Square where
    mirror        = id

instance Invariant Square where
    inv (SQ i)
        | i >= 0    = ok
        | otherwise = bad
                      ["Square with size =", show i, "not allowed"]

-- smart constructor for Square

sq :: Int -> Square
sq = check . SQ

unSQ :: Square -> Int
unSQ (SQ n) = n

type Squares = [Square]

type Width   = Int
type Height  = Int
type Pos     = Int

-- ----------------------------------------

data Rectangle = RT Int Int
                 deriving (Eq, Ord, Show)

instance Figure Rectangle where
    width  (RT w _) = w
    height (RT _ h) = h

instance Area Rectangle where
    area   (RT w h) = w * h

instance Mirror Rectangle where
    mirror (RT w h) = RT h w

instance Invariant Rectangle where
    inv (RT w h)
        | w >= 0 && h >= 0 = ok
        | otherwise        = bad
                             [ "Rectangle with width =", show w
                             , " and height =", show h, "not allowed"
                             ]

-- smart constructor for Rectangle

rect :: Int -> Int -> Rectangle
rect !w !h = check . RT w $ h

type Rectangles = [Rectangle]

-- ----------------------------------------

data Point = PT Pos Pos
             deriving (Eq, Ord, Show)

instance Invariant Point where
    inv (PT x y) 
        | x >= 0 && y >= 0 = ok
        | otherwise        = bad
                             [ "Point with x = ", show x
                             , "and y =", show y, "not allowed"]

instance Figure Point where
    xpos (PT x _) = x
    ypos (PT _ y) = y

instance Mirror Point where
    mirror (PT x y) = PT y x

pt :: Int -> Int -> Point
pt !x !y = check . PT x $ y

-- ----------------------------------------

data PosFig a = FP Point a
              deriving (Eq, Show)

instance Figure a => Figure (PosFig a) where
    width  (FP _ f)        = width  f
    height (FP _ f)        = height f
    xpos   (FP (PT x _) _) = x
    ypos   (FP (PT _ y) _) = y
    xy     (FP p _)        = p

instance Area a => Area (PosFig a) where
    area   (FP _ f)        = area   f

instance Mirror a => Mirror (PosFig a) where
    mirror (FP p f) = FP (mirror p) (mirror f)

type PosRect   = PosFig Rectangle
type PosSquare = PosFig Square

fg :: PosFig a -> a
fg (FP _ f) = f

ptrect :: Int -> Int -> Int -> Int -> PosRect
ptrect !x !y !w !h
    = FP p r
      where
        ! p = pt x y
        ! r = rect w h

ptsq :: Int -> Int -> Int -> PosSquare
ptsq !x !y !w
    = FP p s
      where
        ! p = pt x y
        ! s = sq w

-- ----------------------------------------
--
-- free space is a list of points
-- 
-- free space is is allways a list of points
-- with ascending x coordinates and descending y coordinates
--
-- When the board is rotated 45 degrees left
-- these points form the peaks of a polygon line
-- the valleys of this polygon line between 2 points p1 and p2
-- are the points (x1, y2)
--
-- Squares will only be placed in a valley,
-- which fit into that valley, the left and right corners of
-- the square will never be above the peaks p1 and p2

newtype FreeSpace
    = FS { unFS :: [Point] }
      deriving (Show)

instance Invariant FreeSpace where
    inv s@(FS xs)
        = invFS xs
          where
            invFS []
                = bad ["FreeSpace list is empty", show s]
            invFS [_]
                = ok
            invFS (p1 : ps1@(p2 : _))
                | xpos p1 >= xpos p2
                  ||
                  ypos p1 <= ypos p2
                    = bad ["inconsistent free space: x1 < x2 || y1 > y2 violated for some points p1 and p2", show s]
                | otherwise
                    = invFS ps1

instance Size FreeSpace where
    -- size of booard computed from free space list
    size = ypos . head . unFS

instance Mirror FreeSpace where
    mirror = FS . map mirror . reverse . unFS

instance Area FreeSpace where
    area (FS ps) = sum $ zipWith free ps (tail ps)
                   where
                     n = ypos . head $ ps
                     free (PT x1 _) (PT x2 y2)
                         = (x2 - x1) * (n - y2)

fs :: [Point] -> FreeSpace
fs = check . FS

-- initial free space is a single (n,n) rectangle at (0,0)

initFreeSpace :: Int -> FreeSpace
initFreeSpace n = FS [pt 0 n, pt n 0]

-- final free space: the whole space is covered,
-- so the list of peaks as a single element

nullFreeSpace :: FreeSpace -> Bool
nullFreeSpace (FS [_]) 	= True
nullFreeSpace _         = False

-- largest possible square to be placed
-- all larger square can be removed from the list
-- of unused squares

largestFreeSquare :: FreeSpace -> Int
largestFreeSquare (FS ps)
    = maximum $ 0 : zipWith largest ps (tail ps)
      where
        n = ypos . head $ ps
        largest (PT x1 _) (PT _ y2)
            = (n - x1) `min` (n - y2)

-- largest possible square to be placed without
-- violating freespace invariant.
-- If there isn't any square available, spoiled space
-- must be inserted

largestPossibleSquare :: FreeSpace -> Int
largestPossibleSquare (FS ps)
    = maximum $ 0 : zipWith largest ps (tail ps)
      where
        largest (PT x1 y1) (PT x2 y2)
            = (y1 - y2) `min` (x2 - x1)

-- ----------------------------------------
--
-- remove symmetric search paths
-- assumption: the largest tile is placed in a corner,
-- and this can be done in the 1. step

step1States :: State -> [State]
step1States
    = map rmLarge . nextStates1
    where
      rmLarge s'
          = s' { unusedTiles = dropWhile (> sq1) . unusedTiles $ s' }
            where
              sq1 = fg . head . usedTiles $ s'


-- 2. step: place a tile on top of the 1. tile
-- and mirror the board.
-- ==> in the 3. step a tile <= tile 2 can be placed on top

step2States :: State -> [State]
step2States
    = map mirror . nextStates1

nextMirroredStates :: State -> [State]
nextMirroredStates
    = map mirror . nextStates

step3States :: State -> [State]
step3States s2
    = filter smallerSq2 . nextStates1 $ s2
      where
        sq2 = fg . head . usedTiles $ s2
        smallerSq2
            = (<= sq2) . fg . head . usedTiles

nextStates1 :: State -> [State]
nextStates1 = nextStates' placeSquare1

nextStates :: State -> [State]
nextStates = nextStates' placeSquare

nextStates' :: (Int -> State -> [State]) -> State -> [State]
nextStates' placef s0
    | null sqs
        = mzero
    | otherwise
        = do n <- fmap size . nubSq $ sqs
             placef n s0
    where
      sqs = unusedTiles s0

placeSquare1 :: Int -> State -> [State]
placeSquare1 = placeSquare' placeSq

placeSquare :: Int -> State -> [State]
placeSquare = placeSquare' placeSqAll

placeSquare' :: (Int -> [Point] -> [(PosSquare, [Point])]) -> Int -> State -> [State]
placeSquare' placef n s0
    = do (tp, fs1) <- placef n (unFS . freeSpace $ s0)
         return .
           rmLargestSq .
           insertSpoiled $
           s0 { freeSpace    = fs fs1
              , usedTiles    = tp : usedTiles s0
              , unusedTiles  = rmSq (sq n) $ unusedTiles s0
              , coveredSpace = n * n + coveredSpace s0
              }

insertSpoiled :: State -> State
insertSpoiled s0
    | null sqs
        = s0 { spoiledSpace = n * n - coveredSpace s0 }
    | otherwise
        = placeSpoiled' s0
    where
      n    = size s0
      sqs  = unusedTiles s0
      smin = size . last $ sqs

      placeSpoiled' s'
          = s' { freeSpace    = fs fs1'
               , spoiledRect  = spoiled ++ spoiledRect s'
               , spoiledSpace = (sum . map area $ spoiled) + spoiledSpace s'
               }
                where
                  fs' = unFS . freeSpace $ s'
                  (spoiled, fs1') = placeSpaceInCorners smin fs'

rmLargestSq :: State -> State
rmLargestSq s0
    = s0 { unusedTiles = rm (sq . largestFreeSquare . freeSpace $ s0) $ unusedTiles s0 }
      where
        rm x = dropWhile (> x)

placeLargeSquare :: Int -> State -> [State]
placeLargeSquare i s0
    = do (spoiled, ps) <- placeSpoiled n i (unFS . freeSpace $ s0)
         placeSquare i $
           s0 { freeSpace = fs ps
              , spoiledRect = spoiled ++ spoiledRect s0
              , spoiledSpace = (sum . map area $ spoiled) + spoiledSpace s0
              }
    where
      n = size s0

nextLargeStates :: State -> [State]
nextLargeStates
    = nextStates `orElse` forcePlaceSmallest

forcePlaceSmallest :: State -> [State]
forcePlaceSmallest s0
    = placeLargeSquare i s0
    where
      i = size . last . unusedTiles $ s0

-- ----------------------------------------

rmSq :: Square -> Squares -> Squares
rmSq x
    = rm id
    where
      rm res []
          = res []
      rm res xs@(x1 : xs1)
          | x == x1
              = res xs1
          | x > x1
              = res xs 
          | otherwise
              = rm (res . (x1:)) xs1

nubSq :: [Square] -> [Square]
nubSq (x1 : xs2@(x2 : _))
    | x1 == x2
        = nubSq xs2
    | otherwise
        = x1 : nubSq xs2
nubSq xs
    = xs

-- ----------------------------------------

placeSpoiled :: Int -> Int -> [Point] -> [([PosRect], [Point])]
placeSpoiled sz n ps
    = do cand <- part ps
         addSpace sz n cand

part :: [Point] -> [([Point], Point, [Point])]
part (p0 : ps0)
    = part' [mirror p0] ps0
      where
        part' ls ps1@(p1 : ps2)
            = (ls, pt x y, ps1) : part' ls2 ps2
              where
                x   = ypos . head $ ls
                y   = ypos p1
                ls2 = mirror p1 : ls
        part' _ls _ps
            = []

part _
    = error "part called with empty list"

addSpace :: Int -> Int -> ([Point], Point, [Point]) -> [([PosRect], [Point])]
addSpace sz n (ls@(PT y' _ : _), PT x0 y0, ps@(PT x' _ : _))
    | x0 + n > sz
      ||
      y0 + n > sz
          = mzero	-- to less space to place tile of size n
    | otherwise
        = return (map mirror lfs ++ rfs, (reverse . map mirror $ ls') ++ ps')
    where
      (rfs, ps') = insertSpace (n - (x' - x0)) y0 ps
      (lfs, ls') = insertSpace (n - (y' - y0)) x0 ls
      insertSpace i h0 xs@(PT x y : xs1)
          | i <= 0
              = ([], xs)
          | i <  w1
              = ([ptrect x y1 i (h0 - y1)], pt (x + i) h0 : xs1)
          | i == w1
              = ([ptrect x y1 i (h0 - y1)], pt x1 (y1 + i) : xs2)
          | i >  w1
              = (ptrect x y1 w1 (h0 - y1) : fs', xs')
          where
            (PT x1 y1 : xs2) = xs1
            w1 = x1 - x
            (fs', xs') = insertSpace (i - w1) h0 xs1

-- place a square on top of the used space

placeSq :: Int -> [Point] -> [(PosSquare, [Point])]
placeSq n (p1@(PT x1 y1) : ps2@((PT x2 y2) : ps3))
    | w == n && h == n		-- perfect fit of square, remove p1 and p2 and replace by upper right corner
        = r $ pt x2 y1 : ps3

    | w == n && h >  n		-- perfect fit of width, move p2 up
        = r $ p1 : pt x2 (y2 + n) : ps3

    | w >  n && h == n		-- perfect fit of height, move p1 to the right
        = r $ pt (x1 + n) y1 : ps2

    | w >  n && h >  n		-- square fits, but too small, insert new corner
        = r $ p1 : pt (x1 + n) (y2 + n) : ps2
    where
        w = x2 - x1
        h = y1 - y2
        p = ptsq x1 y2 n
        r x = return (p, x)

placeSq _ _
    = mzero

-- place a square on every possible place

placeSqAll :: Int -> [Point] -> [(PosSquare, [Point])]
placeSqAll n ps1@(p1 : ps2@(_ : _))
    = placeSq n ps1
      `mplus`
      map (second (p1:)) (placeSqAll n ps2)

placeSqAll _ _
    = mzero


-- place spoiled space on left side top, if height is smaller than smallest square

placeSpaceLeftTop :: Int -> [Point] -> ([PosRect], [Point])
placeSpaceLeftTop n (PT x1 y1 : PT x2 y2 : ps3)
    | h < n
        = (ptrect x1 y2 w h : sp1', ps1')
    where
      w = x2 - x1
      h = y1 - y2
      ps' = pt x2 y1 : ps3
      (sp1', ps1') = placeSpaceLeftTop n ps'

placeSpaceLeftTop _ ps
    = ([], ps)


-- place spoiled space on right side bottom, if height is smaller than smallest square

placeSpaceRightBottom :: Int -> [Point] -> ([PosRect], [Point])
placeSpaceRightBottom n
    = (map mirror *** mirror') . placeSpaceLeftTop n . mirror'
    where
      mirror' = unFS . mirror . fs

-- place spoiled space in both corners

placeSpaceInCorners :: Int -> [Point] -> ([PosRect], [Point])
placeSpaceInCorners n ps
    = (s1 ++ s2, ps2)
    where
      (s1, ps1) = placeSpaceLeftTop     n ps
      (s2, ps2) = placeSpaceRightBottom n ps1

-- ----------------------------------------

data State
    = State
      { freeSpace    :: FreeSpace
      , usedTiles    :: [PosSquare]
      , unusedTiles  :: Squares
      , coveredSpace :: Int
      , spoiledSpace :: Int
      , spoiledRect  :: [PosRect]
      }
    deriving (Show)

instance Size State where
    size = size . freeSpace

instance Area State where
    area   = sum . map area . usedTiles

instance Mirror State where
    mirror s
        = s { freeSpace   =     mirror . freeSpace   $ s
            , usedTiles   = map mirror . usedTiles   $ s
            , spoiledRect = map mirror . spoiledRect $ s
            }

instance Invariant State where
    inv s
        = inv (freeSpace s)
          .&&.
          invCond (free + spoiled + covered == total)
                  [ "State: free space + spoiled space + covered space /= board size"
                  , show free, "+", show spoiled, "+", show covered, "/=", show total
                  ]
          .&&.
          invCond (covered' == covered)
                  [ "State: covered space does not match covered in state"
                  , show covered', "/=", show covered
                  ]
          .&&.
          invCond (places == covered)
                  [ "State: some squares placed on the board overlap."
                  , "covered =", show covered, ", places =", show places
                  , "squares = ", show (usedTiles s)
                  ]
          .&&.
          invCond (all fitOnBoard $ usedTiles s)
                  [ "State: some squares don't fit on board."
                  , "squares = ", show (usedTiles s)
                  ]
          .&&.
          mzero
          where
            n        = size $ s
            total    = n * n
            free     = areaUnused s
            spoiled  = spoiledSpace s
            covered' = area s
            covered  = coveredSpace s
            places  = sum . map (length . coords) . usedTiles  $ s
            fitOnBoard f
                = all (\ (x, y) -> x >= 0 && x < n && y >= 0 && y < n) $ coords f

initState :: Int -> Squares -> State
initState n sqs
    = State
      { freeSpace    = initFreeSpace n
      , usedTiles    = []
      , unusedTiles  = reverse . L.sort $ sqs
      , coveredSpace = 0
      , spoiledSpace = 0
      , spoiledRect  = []
      }

finalState :: State -> Bool
finalState s
    = (nullFreeSpace . freeSpace $ s)
      ||
      (null . unusedTiles $ s)

-- ----------------------------------------

-- spoiled space (no longer usable space) is worse than not yet used space
-- this handicap can be raised to perform a broader search

quality :: State -> Int
quality s
    = 10 * (bad1 + bad2) - (good1 + good2)
    where
      bad1  = 4 * spoiledSpace s                  -- spoiling tiles is 4 times worse than placing tiles
      bad2  = n * (length . unFS . freeSpace $ s) -- less peaks (corners) gives better quality
      good1 = 1 * coveredSpace s                  -- more covered gives better quality 
                                                  -- largest placeable square as bonus added
      good2 = 1 * let i = head . (++ [0]) . map size . take 1 . unusedTiles $ s in i * i
      n  = size s

areaAvailable :: State -> Int
areaAvailable
    = sum . map area . unusedTiles

areaCovered :: State -> Int
areaCovered
    = sum . map area . usedTiles

areaUnused :: State -> Int
areaUnused s
    = size s - coveredSpace s - spoiledSpace s

-- ----------------------------------------
--
-- show function for a state and board

showState :: State -> String
showState s
    = unlines $
      [ "board        :"
      , showBoard s
      , "usable tiles : " ++ (show . map width . unusedTiles $ s)
      , "size         : " ++ (show . size $ s)
      , "covered space: " ++ (show . coveredSpace $ s)
      , "free space   : " ++ (show . areaUnused $ s)
      , "spoiled space: " ++ (show . spoiledSpace $ s)
      , "spoiled rect.: " ++ (unwords . map showFigure . spoiledRect $ s)
      , "quality      : " ++ (show . quality $ s)
      , "final state  : " ++ (show . finalState $ s)
      , ""
      ]

showFigure :: Figure f => f -> String
showFigure f
    = show ((xpos f, ypos f), (width f, height f))

type Board = M.Map (Int, Int) Char

showBoard :: State -> String
showBoard s
    = ("+" ++ replicate n '-' ++ "+\n" )
      ++
      (concat . map (("|" ++) .(++ "|\n")) . reverse . toStrings
       $ M.toList . insertSS rs . insertCS cs . insertFig (if null . unusedTiles $ s then "X" else ".") (sq n) $ M.empty
      )
      ++
      ("+" ++ replicate n '-' ++ "+\n" )
    where
      n  = size s
      cs = usedTiles s
      rs = spoiledRect $ s

      insertCS cs' b'
          = foldl ins b' cs'
            where
              ins b'' f
                  = insertFig (reverse . show . width $ f) f b''
              
      insertSS rs' b'
          = foldl ins b' rs'
            where
              ins = flip (insertFig "X")

      toStrings []
          = []
      toStrings xs
          = map snd y : toStrings ys
            where
              (y, ys) = splitAt n xs

insertFig :: Figure f => String -> f -> Board -> Board
insertFig s f b
    = foldl ins b $
      zip (coords f)
          (concat . repeat $ s)
      where
        ins b' ((x', y'), c')
            = M.insert (y', x') c' b'

printST :: SearchState -> IO ()
printST sst
    = do mapM_ (putStrLn . showState) . toList . candidates $ sst
    where
      toList q
          = case H.viewMin q of
              Nothing -> []
              Just (e, q') -> H.payload e : toList q'

showSearchState :: SearchState -> String
showSearchState sst
    = unlines $
      (map showState . reverse. take n . toList . candidates $ sst)
      ++
      [ "search step  : " ++ (show . stepCnt $ sst)
      , "# candidates : " ++ show qs
      , "best " ++ show n ++ " boards"
      , ""
      ]
      ++
      [ "best solution: " ++ (show . bestSolution $ sst)
      , "solution cnt : " ++ (show . solutionCnt  $ sst)
      , "covered space: " ++ (show . coveredArea  $ sst)
      , "spoiled space: " ++ (show . spoiledArea  $ sst)
      ]
    where
      qs = H.size . candidates $ sst
      n = 5 `min` qs
      toList q
          = case H.viewMin q of
              Nothing -> []
              Just (e, q') -> H.payload e : toList q'

-- ----------------------------------------

data SearchState
    = SearchState
      { bestSolution  :: [PosSquare]
      , solutionCnt   :: Int
      , coveredArea   :: Int
      , candidates    :: Candidates
      , queueCapacity :: Int
      , squareSize    :: Int
      , stepCnt       :: Int
      }
    deriving (Show)

instance Invariant SearchState where
    inv s
        = invCond (covered == coveredArea s)
                  [ "SearchState: covered of best solution /= sum of tile areas"]
          .&&.
          invCond (all (not . finalState . H.payload) . H.toUnsortedList . candidates $ s)
                  [ "SearchState: candidate queue contains final states" ]
          .&&.
          invCond (H.size q <= queueCapacity s)
                  [ "searchState: candidate queue exceeds maximum capacity" ]
        where
          q       = candidates s
          covered = sum . map area . bestSolution $ s

type Candidates = H.Heap Candidate
type Candidate = H.Entry Int State

initSearchState :: Width -> Squares -> SearchState
initSearchState n sqs
    = SearchState
      { bestSolution  = []
      , solutionCnt   = 0
      , coveredArea   = 0
      , candidates    = H.singleton . mkCand $ initState n sqs
      , queueCapacity = 1000000
      , squareSize    = n
      , stepCnt       = 0
      }

spoiledArea :: SearchState -> Int
spoiledArea s
    = n * n - coveredArea s
      where
        n = squareSize s

-- ----------------------------------------

type StateFilter = Filter State State

run :: Int -> [StateFilter] -> SearchState -> IO SearchState
run maxSteps places0 sst0
   = do sst1 <- go places0 sst0
        printSearchState sst1
        return sst1
    where
      go :: [StateFilter] -> SearchState -> IO SearchState
      go (place : places) sst
          | steps >= maxSteps
              = return sst
          | otherwise
              = maybe
                (return sst)
                ({- printS0 >=> -} uncurry nextStates' >=> printS >=> go places) nextCand
          where
            steps
                = stepCnt sst

            printS0 x
                = do hPutStrLn stderr "expanding state"
                     hPutStrLn stderr . show . fst $ x
                     return x

            printS sst'
                | steps' `mod` 10000 == 0
                    = do printSearchState sst'
                         return sst'
                | otherwise
                    = return sst'
                where
                  steps' = stepCnt sst'

            nextCand = H.viewMin . candidates $ sst

            nextStates' best rest
                = foldM insertCandidate sst' $ place $ H.payload best
                  where
                    sst' = sst { candidates = rest
                               , stepCnt = stepCnt sst + 1
                               }

            insertCandidate st c
                | tooBad				-- too much spoiled space, cut off search tree
                    = return st
                | otherwise
                    = checkBest c st >>= addCandidate c
                where
                  tooBad
                      = sp >= sa
                        ||
                        n * n - (coveredSpace c + (sum . map area . unusedTiles $ c)) >= sa
                      where
                        sp = spoiledSpace c
                        sa = spoiledArea st
                        n  = squareSize  st

            checkBest c st
                | moreCovered
                    = do writeSolution $ usedTiles c
                         return $
                           st { bestSolution = usedTiles    c
                              , solutionCnt  = solutionCnt  st + 1
                              , coveredArea  = coveredSpace c
                              }
                | otherwise
                    = return st
                where
                  moreCovered
                      = coveredSpace c > coveredArea st

            addCandidate c st
                | finalState c				-- candidate is thrown away
                    = return st
                | otherwise 				-- not final && not tooBad
                    = return $
                      st { candidates = H.insert (mkCand c) (candidates st)
                         }

runL :: Int -> SearchState -> IO SearchState
runL mx = run mx $ step1States : step2States : step3States : repeat nextLargeStates


nullCands :: Candidates -> Bool
nullCands = H.null

mkCand :: State -> Candidate
mkCand s = H.Entry (quality s) s

-- ----------------------------------------

main :: IO ()
main
    = do (z, sqs) <- readProblem
         runProblem steps z sqs
         return ()
    where
      steps :: Int
      steps = 2 * 10^(6::Int)

runProblem :: Int -> Int -> Squares -> IO ()
runProblem steps sqz sqs
    = runL steps s >> return ()
    where
      s = initSearchState sqz sqs

readProblem :: IO (Int, Squares)
readProblem
    = do sqs <- fmap (map sq . reverse . L.sort . read) getLine
         sqz <- fmap read getLine
         return (sqz, sqs)

writeSolution :: [PosSquare] -> IO ()
writeSolution cs
    = do putStrLn . show . map (\ (FP (PT x y) (SQ w)) -> (x, y, w)) $ cs
         hFlush stdout

printSearchState :: SearchState -> IO ()
printSearchState s
    = do hPutStrLn stderr $ showSearchState s
         hFlush stderr

-- ----------------------------------------
{-
s00 = initState 10 (map sq [1,2,3,7,8,9])

-- error inconsistent FS when calling nextsteps

ss1 = State
      { freeSpace = FS {unFS = [PT 9 20,PT 10 14,PT 11 13,PT 20 0]}
      , usedTiles = [FP (PT 9 13) (SQ 1),FP (PT 9 11) (SQ 2),FP (PT 0 11) (SQ 9),FP (PT 0 0) (SQ 11)]
      , unusedTiles = [SQ 8,SQ 7,SQ 6,SQ 5,SQ 4,SQ 3]
      , coveredSpace = 207
      , spoiledSpace = 0
      , spoiledRect = []
      }

ss2 = State
      { freeSpace = FS {unFS = [PT 12 20,PT 13 16,PT 14 10,PT 20 9]}
      , usedTiles = [FP (PT 8 16) (SQ 4)
                    ,FP (PT 13 9) (SQ 1)
                    ,FP (PT 8 11) (SQ 5)
                    ,FP (PT 11 9) (SQ 2)
                    ,FP (PT 0 11) (SQ 8)
                    ,FP (PT 11 0) (SQ 9)
                    ,FP (PT 0 0) (SQ 11)
                    ]
      , unusedTiles = [SQ 7,SQ 6,SQ 3]
      , coveredSpace = 312
      , spoiledSpace = 8
      , spoiledRect = [FP (PT 0 19) (RT 8 1)]
      }

testAddSpace = addSpace 20 3 $ ([PT 10 14,PT 16 13,PT 20 12],PT 14 9,[PT 20 9])

-- -}

run50 = runProblem 10000 50 (map sq [1..49])

-- ----------------------------------------
