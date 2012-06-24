{-# LANGUAGE BangPatterns #-}

module Main
where

import Control.Monad	( mzero
                        , mplus
                        , (>=>)
                        , liftM
                        , foldM
                        )
import Control.Arrow    ( (&&&) )

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
    check x = case inv x of
                Nothing -> x
                Just e  -> error e

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

    area   :: a -> Int
    area f = xpos f * ypos f

    xpos   :: a -> Pos
    xpos _ = 0

    ypos   :: a -> Pos
    ypos _ = 0

    xy     :: a -> Point
    xy f   = PT (xpos f) (ypos f)

    wh     :: a -> Size
    wh f   = SZ (width f) (height f)
  
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

newtype Square = SQ Int
    deriving (Eq, Ord, Show)

instance Figure Square where
    width  (SQ i) = i
    height (SQ i) = i
    area   (SQ i) = i * i

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
    area   (RT w h) = w * h

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


pt :: Int -> Int -> Point
pt !x !y = check . PT x $ y

data Size = SZ Width Height
             deriving (Eq, Ord, Show)

instance Invariant Size where
    inv (SZ w h) 
        | w >= 0 && w >= 0 = ok
        | otherwise        = bad
                             [ "Size with w = ", show w
                             , "and h =", show h, "not allowed"]


sz :: Int -> Int -> Size
sz !w !h = check . SZ w $ h

-- ----------------------------------------

data PosFig a = FP Point a
              deriving (Eq, Show)

instance Figure a => Figure (PosFig a) where
    width  (FP _ f)        = width  f
    height (FP _ f)        = height f
    area   (FP _ f)        = area   f
    xpos   (FP (PT x _) _) = x
    ypos   (FP (PT _ y) _) = y
    xy     (FP p _)        = p

type PosRect   = PosFig Rectangle
type PosSquare = PosFig Square

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

data State
    = State
      { freeSpace    :: FreeSpace
      , usedTiles    :: CoveredSpace
      , unusedTiles  :: Squares
      , coveredSpace :: Int
      , spoiledSpace :: Int
      }
    deriving (Show)

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
            n        = width . freeSpace $ s
            total    = n * n
            free     = areaUnused s
            spoiled  = spoiledSpace s
            covered' = area s
            covered  = coveredSpace s
            places  = sum . map (length . coords) . usedTiles  $ s
            fitOnBoard f
                = all (\ (x, y) -> x >= 0 && x < n && y >= 0 && y < n) $ coords f

instance Figure State where
    width  = width . freeSpace
    height = height . freeSpace
    area   = sum . map area . usedTiles

initState :: Int -> Squares -> State
initState n sqs
    = State
      { freeSpace    = initFreeSpace n
      , usedTiles = []
      -- , unusedTiles  = filter (\ f -> let w = width f in 0 <= w && w <= n) sqs
      -- , unusedTiles  = map sq  [ n - i | i <- [0 .. n - 1]]
      -- , unusedTiles  = map sq  [ n - i | i <- [1 .. n - 1]]
      , unusedTiles = reverse . L.sort $ sqs
      , coveredSpace = 0
      , spoiledSpace = 0
      }

finalState :: State -> Bool
finalState s
    = (nullFreeSpace . freeSpace $ s)
      ||
      (null . unusedTiles $ s)

classicProblem :: Int -> (Int, Squares)
classicProblem n
    = (n, map sq  [ n - i | i <- [1 .. n - 1]])

-- ----------------------------------------

-- spoiled space (no longer usable space) is worse than not yet used space
-- this handicap can be raised to perform a broader search

quality :: Int -> State -> Int
quality weight s
    = 2 * weight * spoiledSpace s - coveredSpace s

areaCovered :: State -> Int
areaCovered
    = sum . map area . usedTiles

areaUnused :: State -> Int
areaUnused
    = sum . map area . unFS . freeSpace

whCurrFreeSpace :: State -> Size
whCurrFreeSpace
    = wh . currFreeSpace . freeSpace

-- ----------------------------------------
--
-- state transtions

-- remove useless squares
-- this can result in a final state, if all squares are removed

removeUselessTiles :: Width -> State -> State
removeUselessTiles n s
    | null usableTiles
        = s { freeSpace    = FS [] [ptrect 0 n n 0]
            , unusedTiles  = []
            , spoiledSpace = n * n - coveredSpace s
            }
    | otherwise
        = s { unusedTiles = usableTiles }
    where
      usableTiles
          = dropWhile (\ f -> width f > (largestPossibleSquare . unFS $ freeSpace s))
            $ unusedTiles s

-- insert spoiled space for
-- rectangles or parts of rectangles that can't be covered by tiles
-- because all tiles are too large

removeSpoiledSpace :: State -> State
removeSpoiledSpace s
    | null sqs				-- no more tiles: final state
        = s
    | smallestSquareSize == 1		-- smallest tile can be placed anywhere
        = s
    | otherwise
        = s { freeSpace    = FS [] fs1
            , spoiledSpace = spoiledSpace s + sps
            }
    where
      fs0 = unFS . freeSpace $ s
      r1  = head fs0
      h1  = height r1
      sqs = unusedTiles s

      smallestSquareSize = width . last $ sqs

      (fs1, sps) = removeSpoiled smallestSquareSize h1 fs0

      removeSpoiled minWidth leftWall' rs0'@(r0 : rs0)
          | h0 == 0       = cons		-- no space to remove
          | h0 < minWidth = remove h0		-- rectangle can be removed, height too small
          | w0 < minWidth
            &&
            minWall' > 0  = remove minWall'     -- rectangle can be filled up
          | otherwise     = cons
          where
            x0 = xpos   r0
            y0 = ypos   r0
            w0 = width  r0
            h0 = height r0

            rightWall' = rightWall (FS [] rs0')
            minWall'   = (leftWall' `min` rightWall')
            (rs', sp') = removeSpoiled minWidth (0 - rightWall') rs0
            cons       = (r0  : rs', sp')
            remove h'  = (r0' : rs', sp' + a0')
                where
                  a0'  = h' * w0
                  r0'  = ptrect x0 (y0 + h') w0 (h0 - h')

      removeSpoiled _ _ _
          = ([], 0)

mergeFreeSpace :: State -> State
mergeFreeSpace s
    = s { freeSpace = FS [] . merge' . unFS . freeSpace $ s
        }
      where
        merge' (x : xs@( _ : _)) = merge x xs
        merge' xs                = xs

        merge x []
            = [x]
        merge r1 (r2 : rs)
            | h1 == h2  = merge (ptrect x1 y1 (w1 + w2) h1) rs
            | otherwise =  r1 : merge r2 rs
            where
              PT x1 y1 = xy r1
              SZ w1 h1 = wh r1
              SZ w2 h2 = wh r2

moveToMin :: State -> State
moveToMin s
    = s { freeSpace = moveToValley $ freeSpace s }

-- ----------------------------------------

type StateFilter = Filter State State

place :: Width -> StateFilter -> StateFilter
place n f s
    = do r <- f s
         return . check . moveToMin . mergeFreeSpace . removeSpoiledSpace . removeUselessTiles n $ r

placeSquare :: (Square -> FreeSpace -> FreeSpace) -> Square -> StateFilter
placeSquare placeFigure q s
    | wq <= w `min` h
      &&
      q `elem` usqs
          = return $
            s { freeSpace
                  = placeFigure q fsp
              , usedTiles
                  = ptsq x y wq : usedTiles s
              , coveredSpace
                  = area q + coveredSpace s
              , unusedTiles
                  = removeSquare q (unusedTiles s)
              }
    | otherwise
        = mzero
    where
      wq   = width q
      usqs = unusedTiles s
      fsp  = freeSpace s
      f    = currFreeSpace fsp
      PT x y = xy f
      SZ w h = wh f

placeSquareL :: Square -> StateFilter
placeSquareL = placeSquare placeFigureL

placeSquareR :: Square -> StateFilter
placeSquareR = placeSquare placeFigureR

placeSquareLR :: Square -> StateFilter
placeSquareLR q s
    | wq == w
        = placeSquareL q s
    | wq <  w
        = placeSquareL q s
          ++
          placeSquareR q s
    | otherwise
        = []
    where
      wq   = width q
      w    = width . currFreeSpace . freeSpace $ s


placeFitWidth :: StateFilter
placeFitWidth s
    = placeSquareL (sq w) s
      where
        SZ w _  = whCurrFreeSpace s

-- --------------------

placeAny' :: (Square -> StateFilter) -> (Int -> [Int]) -> StateFilter
placeAny' placeSquare' gen s
    = do q <- qs
         placeSquare' q s
    where
      SZ w h = whCurrFreeSpace $ s
      qs     = filterAvailable s $
               gen (w `min` h)

placeAnyL :: (Int -> [Int]) -> StateFilter
placeAnyL = placeAny' placeSquareL

placeAnyLR :: (Int -> [Int]) -> StateFilter
placeAnyLR = placeAny' placeSquareLR

placeAnyAltN :: StateFilter
placeAnyAltN s
    = placeAnyL (genAltSeqN lw) s
      where
        lw = leftWall . freeSpace $ s

placeAnyAlt2 :: StateFilter
placeAnyAlt2 = placeAnyL genAltSeq2

placeAnyDescL :: StateFilter
placeAnyDescL = placeAnyL genDescSeq

placeAnyDescLR :: StateFilter
placeAnyDescLR = placeAnyLR genDescSeq

placeHole :: StateFilter
placeHole s
    | h > 0
        = return $
          s { freeSpace
                = placeFigureL rt fsq
            , spoiledSpace
                = area rt + spoiledSpace s
            }
    | otherwise
        = mzero
    where
      fsq    = freeSpace s
      h      = uncurry min . (leftWall &&& rightWall) $ fsq
      SZ w _ = whCurrFreeSpace s
      rt     = rect w h

placeHole1 :: StateFilter
placeHole1 s
    | h > 0
        = return $
          s { freeSpace
                = placeFigureL rt fsq
            , spoiledSpace
                = area rt + spoiledSpace s
            }
    | otherwise
        = mzero
    where
      fsq    = freeSpace s
      h      = uncurry min . (leftWall &&& rightWall) $ fsq
      rt     = rect 1 1

placeAnyAltNOrHole :: StateFilter
placeAnyAltNOrHole
    = placeAnyAltN `orElse` placeHole


placeAnyOrHole1 :: StateFilter -> StateFilter
placeAnyOrHole1 f s
    | null ss			-- no chance: place spoiled place
        = placeHole s
    | otherwise
        = ss -- ++ placeHole1 s	-- there are chances to place a square,
                                -- but inserting a 1*1 hole could also be a choice
      where
        ss = f s		-- try to place squares

-- ----------------------------------------
--
-- different generator for selecting square candidates

genDescSeq :: Int -> [Int]
genDescSeq n
    = reverse [1 .. n]

genAltSeqN :: Int -> Int -> [Int]
genAltSeqN i n
    = take n . filter (\ x -> 0 < x && x <= n) . map (+ i) $ altSeq

genAltSeq2 :: Int -> [Int]
genAltSeq2 n
    = genAltSeqN n0 n
      where
        n0 = (n + 1) `div` 2 

altSeq :: [Int]
altSeq = gen 0
    where
      gen i = i : gen i1
          where
            i1 | i > 0     = (-i)
               | otherwise = (1 - i)

filterAvailable :: State -> [Int] -> [Square]
filterAvailable s
    = filter (`elem` unusedTiles s) . map sq

-- ----------------------------------------

removeSquare :: Square -> Squares -> Squares
removeSquare _ []
    = []
removeSquare x ss@(s : ss1)
    | x <  s
        = s : removeSquare x ss1
    | x == s
        = ss1
    | otherwise
        = ss

-- ----------------------------------------
--
-- free space is a navigatable list of rectangles
-- without any space in between, the head of rr
-- is the currently processed space, for which
-- tiles will be searched to cover its bottom

data FreeSpace    = FS { ll :: [PosRect]
                       , rr :: [PosRect]
                       }
                    deriving (Show)

instance Invariant FreeSpace where
    inv s@(FS _ [])
        = bad ["FreeSpace list is empty on right side", show s]
    inv s
        = invfs 0 (-1) lrs
          where
            lrs@(r1:_) = unFS s
            n          = ypos r1 + height r1
            invfs _ _ []
                = ok
            invfs x1 h1 (r : rs1)
                | x1 == x
                  &&
                  h1 /= h
                  &&
                  y + h == n
                  &&
                  w > 0
                      = invfs (x + w) h rs1
                | otherwise
                    = bad
                      ["inconsistent FreeSpace", show s]
                where
                  PT x y = xy r
                  SZ w h = wh r

instance Figure FreeSpace where
    width  = sum . map width . unFS
    height = (\ f -> ypos f + height f) . head . unFS
    area   = sum . map area . unFS

fs :: [PosRect] -> FreeSpace
fs = check . FS []

unFS :: FreeSpace -> [PosRect]
unFS (FS l r)
    = foldl (flip (:)) r l

type CoveredSpace = [PosSquare]

-- ----------------------------------------

-- initial free space is a single (n,n) rectangle at (0,0)

initFreeSpace :: Int -> FreeSpace
initFreeSpace n = FS [] [ptrect 0 0 n n]

-- final free space: the whole space is covered,
-- so the rectangle is located at (0,n) and has size (n, 0) 

nullFreeSpace :: FreeSpace -> Bool
nullFreeSpace (FS [] [r]) = height r == 0
nullFreeSpace _           = False

-- current rectangle in free space list

currFreeSpace :: FreeSpace -> PosRect
currFreeSpace
    = head . rr

-- heights of borders of a free space rectangle
-- values may be negative

leftWall :: FreeSpace -> Int
leftWall (FS [] (m : _))
    = height m
leftWall (FS (l : _) (m : _))
    = height m - height l
leftWall s
    = illegal "leftWall" s

rightWall :: FreeSpace -> Int
rightWall (FS _ (m : []))
    = height m
rightWall (FS _ (m : r : _))
    = height m - height r
rightWall s
    = illegal "rightWall" s

-- a free space with real walls around
-- this is a candidate to be filled

inValley :: FreeSpace -> Bool
inValley s
    = leftWall s `min` rightWall s > 0
      ||
      nullFreeSpace s

-- ----------------------------------------
--
-- place a figure at the left bottom corner of a free rectangle space
-- and try to merge free space with the neighbour space

placeFigureL :: Figure f => f -> FreeSpace -> FreeSpace
placeFigureL f (FS ls (r : rs))
    = FS ls (takeSpaceL f r ++ rs)
    -- = fuseSpace $ FS ls (takeSpaceL f r ++ rs)
placeFigureL _ s
    = illegal "placeFigureL" s


-- place a figure at the right bottom corner of a free rectangle space
-- and try to merge free space with the neighbour space

placeFigureR :: Figure f => f -> FreeSpace -> FreeSpace
placeFigureR f (FS ls (r : rs))
    = FS ls (takeSpaceR f r ++ rs)

{- fusion comes later
    = case takeSpaceR f r of
        [r1', r2'] -> fuseSpace $ FS (r1' : ls) (r2' : rs)
        [r1']      -> fuseSpace $ FS        ls  (r1' : rs)
        e          -> illegal "placeFigureR" (show e)
-}

placeFigureR _ s
    = illegal "placeFigureR" s

-- ----------------------------------------
--
-- navigation in the list of free space squares

moveL :: FreeSpace -> Maybe FreeSpace
moveL (FS [] _)
    = mzero
moveL (FS (l1: ls) rs)
    = return $ FS ls (l1 : rs)

moveR :: FreeSpace -> Maybe FreeSpace
moveR (FS _ ( _ : []))
    = mzero
moveR (FS ls (r : rs))
    = return $ FS (r : ls) rs
moveR s
    = illegal "moveR" s

-- search a valley

moveToValley' :: FreeSpace -> FreeSpace
moveToValley' s
    = fromMaybe (illegal "moveToValley" s) $
      rightValley s `mplus` (moveL >=> leftValley) s
    where
      rightValley s'
          | inValley s' = return s'
          | otherwise    = moveR >=> rightValley $ s'

      leftValley s'
          | inValley s' = return s'
          | otherwise    = moveL >=> rightValley $ s'

moveToValley :: FreeSpace -> FreeSpace
moveToValley
    = minimumBy minFS . buildZippers . unFS

buildZippers :: [PosRect] -> [FreeSpace]
buildZippers
    = build []
    where
      build ls rs@(r1 : rs1)
          = FS ls rs : rest
          where
            rest
                | null rs1  = []
                | otherwise = build (r1 : ls) rs1
      build _ _
          = error "buildZippers with wrong args"

minFS :: FreeSpace -> FreeSpace -> Ordering
minFS (FS _ (r1 : _)) (FS _ (r2 : _))
    | h1 >  h2  = LT
    | h1 <  h2  = GT
    | w1 <  w2  = LT
    | w1 >  w2  = GT
    | otherwise = EQ
    where
      h1 = height r1
      w1 = width  r1
      h2 = height r2
      w2 = width  r2

minFS _ _
    = error "minFS with wrong args"

-- ----------------------------------------
--
-- try to merge adjoin freespace rectangles

fuseSpace :: FreeSpace -> FreeSpace
fuseSpace s@(FS _ [])
    = s
fuseSpace (FS ls rs)
    = fuseLeft ls . fuseRight $ rs
    where
      fuseRight (r1 : r2 : rs')
          = fuseFreeSpace r1 r2 ++ rs'

      fuseRight rs'
          = rs'

      fuseLeft ls' rs'
          | null ls'
            ||
            null rs'
                = FS ls' rs'
          | otherwise
              = FS ls1' (fuseFreeSpace l1' r1' ++ rs1')
                where
                  (l1' : ls1') = ls'
                  (r1' : rs1') = rs'

      fuseFreeSpace r1 r2
          | y1 == y2
              = [ptrect x1 y1 (w1 + w2) h1]
          | otherwise
              = [r1, r2]
          where
            PT x1 y1 = xy r1
            SZ w1 h1 = wh r1
            y2       = ypos   r2
            w2       = width  r2

-- ----------------------------------------
--
-- take space from a free rectangle
-- and split the remaining space into 2 rectangles
-- if necessary
       
takeSpaceL :: Figure f => f -> PosRect -> [PosRect]
takeSpaceL f r
    = ptrect' x (y + hf) wf (h - hf)
      ++
      ptrect' (x + wf) y (w - wf) h
      where
        wf = width  f
        hf = height f
        x  = xpos   r
        y  = ypos   r
        w  = width  r
        h  = height r

takeSpaceR :: Figure f => f -> PosRect -> [PosRect]
takeSpaceR f r
    = ptrect' x y (w - wf) h
      ++
      ptrect' (x + (w - wf)) (y + hf) wf (h - hf)
      where
        wf = width  f
        hf = height f
        x  = xpos   r
        y  = ypos   r
        w  = width  r
        h  = height r

ptrect' :: Pos -> Pos -> Width -> Height -> [PosRect]
ptrect' x' y' w' h'
    | w' == 0   = []
    | otherwise = [ptrect x' y' w' h']

-- ----------------------------------------
--
-- largest square placable at left side of a free space list

largestPossibleSquareL :: [PosRect] -> Int
largestPossibleSquareL []
    = 0

largestPossibleSquareL (f : [])
    = width f `min` height f

largestPossibleSquareL (f1 : f2 : s)
    | w1 >= h1  = h1
    | h2 >  w1  = largestPossibleSquareL (ptrect 0 0 (w1 + w2) (h1 `min` h2) : s)
    | otherwise = w1
    where
      SZ w1 h1 = wh f1
      SZ w2 h2 = wh f2

largestPossibleSquare :: [PosRect] -> Int
largestPossibleSquare
    = maximum . map largestPossibleSquareL . tails

-- ----------------------------------------
--
-- show function for a state and board

showState :: Int -> State -> String
showState n s
    = unlines $
      [ "board        :"
      , showBoard s
      , "usable tiles : " ++ (show . map width . unusedTiles $ s)
      , "size         : " ++ (show . width $ s)
      , "covered space: " ++ (show . coveredSpace $ s)
      , "free space   : " ++ (show . areaUnused $ s)
      , "spoiled space: " ++ (show . spoiledSpace $ s)
      , "current free : " ++ (showFigure . currFreeSpace . freeSpace $ s)
      , "quality      : " ++ (show . quality n $ s)
      , "final state  : " ++ (show . finalState $ s)
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
       . M.toList . insertFS rs . insertCS cs . insertFig "X" (sq n) $ M.empty
      )
      ++
      ("+" ++ replicate n '-' ++ "+\n" )
    where
      n  = width s
      cs = usedTiles s
      rs = unFS . freeSpace $ s

      insertCS cs' b'
          = foldl ins b' cs'
            where
              ins b'' f
                  = insertFig (reverse . show . width $ f) f b''
              
      insertFS rs' b'
          = foldl ins b' rs'
            where
              ins = flip (insertFig ".")

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
    = do mapM_ (putStrLn . showState (squareSize sst)) . toList . candidates $ sst
    where
      toList q
          = case H.viewMin q of
              Nothing -> []
              Just (e, q') -> H.payload e : toList q'

showSearchState :: SearchState -> String
showSearchState sst
    = unlines $
      (map (showState $ squareSize sst) . reverse. take n . toList . candidates $ sst)
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
      { bestSolution  :: CoveredSpace
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
      , candidates    = H.singleton . mkCand n $ initState n sqs
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

run :: StateFilter -> Int -> SearchState -> IO SearchState
run placeSq maxSteps sst0
   = do sst1 <- go sst0
        printSearchState sst1
        return sst1
    where
      size
          = squareSize sst0

      nextCandidates
          = place size placeSq

      go sst
          | steps >= maxSteps
              = return sst
          | otherwise
              = maybe
                (return sst)
                (uncurry nextStates >=> printS >=> go) nextCand
          where
            steps    = stepCnt sst

            printS sst'
                | steps' `mod` 1000 == 0
                    = do printSearchState sst'
                         return sst'
                | otherwise
                    = return sst'
                where
                  steps' = stepCnt sst'

            nextCand = H.viewMin . candidates $ sst

            nextStates best rest
                = foldM insertCandidate sst' $ nextCandidates $ H.payload best
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
                      st { candidates = H.insert (mkCand size c) (candidates st)
                         }

runL :: Int -> SearchState -> IO SearchState
runL = run $ placeAnyOrHole1 placeAnyDescL

{-
stepsL :: Int -> SearchState -> IO SearchState
stepsL n sst
    | n == 0	= return sst
    | otherwise = case step' sst of
                    Nothing -> return sst
                    Just (sst', solution) ->
                        do maybe (return ()) writeSolution $ solution
                           stepsL (n - 1) sst'

-- stepsL (n - 1) . step (placeAnyOrHole1 placeAnyDescL)

stepsLR :: Int -> SearchState -> IO SearchState
stepsLR n
    | n <= 0    = id
    | otherwise = steps (n - 1) . step placeAnyDescL
    where
      steps i
          | i == 0    = id
          | otherwise = steps (i - 1) . step placeAnyDescLR

step :: StateFilter -> SearchState -> Maybe (SearchState, Maybe CoveredSpace)
step placeSq sst
    = undefined
-- -}

-- {-
stepsL :: Int -> SearchState -> SearchState
stepsL n
    | n <= 0	= id
    | otherwise = stepsL (n - 1) . step (placeAnyOrHole1 placeAnyDescL)

stepsLR :: Int -> SearchState -> SearchState
stepsLR n
    | n <= 0    = id
    | otherwise = steps (n - 1) . step placeAnyDescL
    where
      steps i
          | i == 0    = id
          | otherwise = steps (i - 1) . step placeAnyDescLR

step :: StateFilter -> SearchState -> SearchState
step placeSq sst
    = case H.viewMin . candidates $ sst of
        Nothing -> sst
        Just (e, qu) -> let sst' = sst 
                                   { candidates = qu
                                   , stepCnt = stepCnt sst + 1
                                   }
                        in
                          check $
                          (foldl (flip nextCand) sst' .  nextCandidates . H.payload $ e)
      where
        nextCandidates
            = place (squareSize sst) placeSq

        nextCand c st
            | tooBad						-- too much spoiled space, cut off search tree
                = st
            | otherwise
                = addCandidate c . checkBest c $ st
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
                = st
                  { bestSolution = usedTiles    c
                  , solutionCnt  = solutionCnt  st + 1
                  , coveredArea  = coveredSpace c
                  }
            | otherwise
                = st
            where
              moreCovered
                  = coveredSpace c > coveredArea st

        addCandidate c st
            | finalState c				-- candidate is thrown away
                = st
            | otherwise 				-- not final && not tooBad
                = st                                  -- insert candiadate into queue
                  { candidates = H.insert (mkCand n c) (candidates st)
                  }
            where
              n = squareSize st
-- -}

nullCands :: Candidates -> Bool
nullCands = H.null

mkCand :: Int -> State -> Candidate
mkCand n s = H.Entry (quality n s) s

-- ----------------------------------------
{-
choices :: Width -> Squares -> [Squares]
choices n _
    | n < 0  = []
    | n == 0 = [[]]
choices _ [] = []

choices n (sq : sqs)
    | w <= n
        = map (sq:) (choices (n - w) sqs) ++ cs
    | otherwise
        = cs
    where
      w = width sq
      cs = choices n sqs

-- ----------------------------------------

perms xs = [y : ps | (y,ys) <- selections xs, ps <- perms ys]

selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

-- ----------------------------------------

-- {-

sq1 = map sq . reverse $ [1..8]

-- -}


inter x [] = [[x]]
inter x yys@(y:ys) = [x:yys] ++ map (y:) (inter x ys)

permute [] = [[]]
permute (x:xs) = concatMap (inter x) (permute xs)

-- -}

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
    = runL steps s0 >> return ()
    where
      s0 = initSearchState sqz sqs

readProblem :: IO (Int, Squares)
readProblem
    = do sqs <- fmap (map sq . reverse . L.sort . read) getLine
         sqz <- fmap read getLine
         return (sqz, sqs)

writeSolution :: CoveredSpace -> IO ()
writeSolution cs
    = do putStrLn . show . map (\ (FP (PT x y) (SQ w)) -> (x, y, w)) $ cs
         hFlush stdout

printSearchState :: SearchState -> IO ()
printSearchState s
    = do hPutStrLn stderr $ showSearchState s
         hFlush stderr

-- ----------------------------------------
