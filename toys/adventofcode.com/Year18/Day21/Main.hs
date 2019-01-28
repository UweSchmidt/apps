{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns  #-}

-- solution for
-- http://adventofcode.com/2018/day/21

module Main where

import           Data.Bits       ((.&.), (.|.))
import           Data.Char       (toLower)
import           Data.Array.IArray
import           Data.IntSet     (IntSet)
import qualified Data.IntSet     as IS
import           Data.List       (intercalate)

import           Util.Main1      (main12)

import           Control.Arrow   (second)
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.Reader

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char

import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-21"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

withTrace :: Bool
withTrace = False -- True

trace' :: String -> a -> a
trace' s
  | withTrace = trace s
  | otherwise = id

-- ----------------------------------------
--
-- the machine state
--

data Mem = Mem
           { mem :: [Val]
           , ic  ::  Int
           , r5s :: (Val, IntSet)
           }
           deriving (Show, Eq)

type Val       = Int

toMem :: [Val] -> Int -> Mem
toMem ms i = Mem
             { mem = ms
             , ic  = i
             , r5s = (-1, IS.empty)
             }

setMem :: [Val] -> Mem -> Mem
setMem v m = m { mem = v }

bindIc :: Int -> Mem -> Mem
bindIc i m = m { ic = i}

memAt :: Int -> Mem -> Val
memAt i m = mem m !! i

setMemAt :: Int -> Val -> Mem -> Mem
setMemAt i v m@(Mem {mem = mm}) =
  m {mem = set mm}
  where
    set ms =
      ls ++ [v] ++ rs
      where
        (ls, rs) = second (drop 1) . splitAt i $ ms

icVal :: Mem -> Val
icVal m =
  memAt (ic m) m

setIcVal :: Val -> Mem -> Mem
setIcVal v m = setMemAt (ic m) v m

incrIc :: Mem -> Mem
incrIc m =
  setMemAt (ic m) (1 + icVal m) m

logR5 :: Val -> Mem -> Mem
logR5 v m = m { r5s = (v, IS.insert v s1) }
  where
    (_, s1) = r5s m

-- ----------------------------------------
--
-- the program

type Prog      = Array Int MInstr
type MInstr    = (OpCode, (Int, Int, Int))

data OpCode    = Addr | Addi
               | Mulr | Muli
               | Banr | Bani
               | Borr | Bori
               | Setr | Seti
               | Gtir | Gtri | Gtrr
               | Eqir | Eqri | Eqrr
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Ix OpCode where
  range (lb, ub) = [lb .. ub]

  index b@(lb, _ub) i
    | inRange b i  =  fromEnum i - fromEnum lb
    | otherwise    =  error $
                      "index out of range: " ++
                      show i ++ " " ++ show b

  inRange (l, u) i = fromEnum i >= fromEnum l
                     &&
                     fromEnum i <= fromEnum u

-- ----------------------------------------

toProg :: [MInstr] -> Prog
toProg is = listArray (0, len'1) is
  where
    len'1 = length is - 1

incrIC :: Action ()
incrIC = do
  (lb, ub) <- asks bounds
  i'       <- (+ 1) <$> getIC
  if lb <= i' && i' <= ub
    then modify $ setIcVal i'
    else throwError ICoutOfRange

-- ----------------------------------------
--
-- the state monad for running an action

type Action     = ExceptT Terminated (ReaderT Prog (State Mem))

data Terminated = ICoutOfRange | Terminated Int deriving Show

type Instr      = Int -> Int -> Int -> Action ()

type DecodeOp   = Array OpCode Instr


runMachine :: Prog -> ([Val], Int) -> (Either Terminated (), [Val])
runMachine prog m0 =
  second mem $ runState (runReaderT (runExceptT runProg) prog) (uncurry toMem m0)

runProg :: Action ()
runProg = forever (traceInstr28 >> exec1Instr)

exec1Instr :: Action ()
exec1Instr = do
  m0 <- get                          -- for trace
  mi@(oc, (x1, x2, x3)) <- getIns
  (decodeInstr ! oc) x1 x2 x3

  m1 <- get                          -- for trace
  trace' (showMem m0 ++ " " ++ showMInstr mi ++ " " ++ showVals (mem m1)) $ return ()
  incrIC

traceInstr28 :: Action ()
traceInstr28 = do
  i <- getIC
  when (i == 28) $ do
    r5 <- getVal 5
    (lst, vs) <- gets r5s
    trace' (show (IS.size vs) ++ " r5=" ++ show r5) $ return ()
    if (r5 `IS.member` vs)
      then do
           trace' ("duplicate value in r5: " ++ show r5) $
             trace' ("last value in r5: " ++ show lst) $
             throwError $ Terminated lst
      else
       modify (logR5 r5)

getIns :: Action MInstr
getIns = do
  icnt <- getIC
  asks (! icnt)

getIC :: Action Int
getIC = gets icVal

getVal :: Int -> Action Val
getVal i = gets (memAt i)

putVal :: Int -> Val -> Action ()
putVal i v = modify (setMemAt i v)

getImm :: Int -> Action Val
getImm = return

ignArg :: Int -> Action Val
ignArg _ = return $ error "argument should be ignored, but it's evaluated"

execInstr :: (Val -> Val -> Val)   -- the binary operation
          -> (Int -> Action Val)   -- get 1. operand
          -> (Int -> Action Val)   -- get 2. operand
          -> Instr
execInstr op get1 get2 x1 x2 x3 = do
  v1 <- get1 x1
  v2 <- get2 x2
  putVal x3 (v1 `op` v2)

addr, addi
  , mulr, muli
  , banr, bani
  , borr, bori
  , setr, seti
  , gtir, gtri, gtrr
  , eqir, eqri, eqrr :: Instr

addr = execInstr (+)   getVal getVal
addi = execInstr (+)   getVal getImm
mulr = execInstr (*)   getVal getVal
muli = execInstr (*)   getVal getImm
banr = execInstr (.&.) getVal getVal
bani = execInstr (.&.) getVal getImm
borr = execInstr (.|.) getVal getVal
bori = execInstr (.|.) getVal getImm
setr = execInstr const getVal ignArg
seti = execInstr const getImm ignArg
gtir = execInstr (.>.) getImm getVal
gtri = execInstr (.>.) getVal getImm
gtrr = execInstr (.>.) getVal getVal
eqir = execInstr (.=.) getImm getVal
eqri = execInstr (.=.) getVal getImm
eqrr = execInstr (.=.) getVal getVal

infix 4 .>., .=.

(.>.), (.=.) :: Val -> Val -> Val
v1 .>. v2 = fromEnum $ v1 >  v2
v1 .=. v2 = fromEnum $ v1 == v2

decodeInstr :: DecodeOp
decodeInstr =
  listArray (minBound, maxBound)
  [ addr, addi
  , mulr, muli
  , banr, bani
  , borr, bori
  , setr, seti
  , gtir, gtri, gtrr
  , eqir, eqri, eqrr
  ]

-- ----------------------------------------

type Input     = (Int, [MInstr])

type Result2 = Int

-- reverse engineering shows, there is a
-- single instr referencing r0, this is
-- the eqrr 5 0 4 at .28, if this compare gives true
-- the ic is set to .31, which is out of range
--
-- in a trace run with r0 init with 0 the 1. time
-- ic has value .28, r5 is set to 6778585
-- so this is the value, r0 must be initialized
--
-- see day21.ass

solve1 :: Input -> Int
solve1 = solve1' 6778585

solve1' :: Int -> Input -> Int
solve1' r0 (bindIC, is) =
  checkRes . fst . runMachine (toProg is) $ (r0 : replicate 5 0, bindIC)
  where
    checkRes (Left ICoutOfRange  ) = r0
    checkRes (Left (Terminated i)) = i
    checkRes _                     = error "solve1': the impossible happend"

-- this solution is pretty slow
-- the input prog is repeated 12767 times
-- so take your time, about 20 minutes, time for a coffee break

solve2' :: Input -> Int
solve2' = solve1' 0

-- solve2 doesn't use the input prog, but the reverse engineered assember prog
-- loop, which generates all values, which are compared with r0,
-- in this sequence the first duplicate is searched, the value before this
-- is the value searched for

solve2 :: Input -> Int
solve2 _inp =
  dup 0 IS.empty $ loop 0

-- ----------------------------------------

dup :: Int -> IntSet -> [Int] -> Int
dup lst vs (i : is)
  | i `IS.member` vs = lst
  | otherwise        = trace' ("i=" ++ show i) $ dup i (IS.insert i vs) is
dup _ _ _ = error "dup: empty list"

-- starts at .6
loop :: Int -> [Int]
loop !r5 =
  r5' : loop r5'
  where
    r5' = loop1 (r5 .|. 65536) 10362650

-- starts at .8
loop1 :: Int -> Int -> Int
loop1 !r2 !r5
  | r2 < 256  = r5''''
  | otherwise = -- trace' ("r2,r5=" ++ show (r2, r5) ++ " r2',r5'=" ++ show (r2',r5'''')) $
                loop1 r2' r5''''
  where
    r5'    = r5 + (r2 .&. 255)   -- r2 `mod` 256
    r5''   = r5' .&. 16777215    -- r5'' = r5' `mod` 2^24
    r5'''  = r5'' * 65899
    r5'''' = r5''' .&. 16777215  -- `mod` 2^24
    r2'    = r2 `div` 256

-- ----------------------------------------

showOpCode :: OpCode -> String
showOpCode = toL . show
  where
    toL (c : cs) = toLower c : cs
    toL x        = x

showMInstr :: MInstr -> String
showMInstr (oc, (x1, x2, x3)) = unwords
  [ showOpCode oc, show x1, show x2, show x3]

showMem :: Mem -> String
showMem m =
  "." ++ show (mem m !! ic m) ++ " " ++ showVals (mem m)

showVals :: [Val] -> String
showVals vs = "[" ++ intercalate ", " (map show vs)  ++ "]"

-- ----------------------------------------

type SP = Parsec () String

fromString :: String -> Input
fromString s = maybe (error "no parse") id $ parseMaybe pInput s

pInput :: SP Input
pInput = (,) <$> parse1 <*> parse2
  where
    parse1 :: SP Int
    parse1 = string "#ip " *> num <* nl

    parse2 :: SP [MInstr]
    parse2 = many (minstr <* nl)

    minstr :: SP MInstr
    minstr = do
      oc <- opcode <* sp
      x1 <- num    <* sp
      x2 <- num    <* sp
      x3 <- num
      return (oc, (x1, x2, x3))

    num :: SP Int
    num = read <$> some digitChar

    nl = char '\n'
    sp = char ' '

opcode :: SP OpCode
opcode = foldr1 (<|>) $
  map toOc [minBound .. maxBound]
  where
    toOc :: OpCode -> SP OpCode
    toOc i = try (string $ showOpCode i) *> return i


-- ----------------------------------------

inp' :: IO String
inp' = readFile "Year18/Day21/day21.txt"

inp :: String
inp = "#ip 3\nseti 123 0 5\nbani 5 456 5\neqri 5 72 5\naddr 5 3 3\nseti 0 0 3\nseti 0 5 5\nbori 5 65536 2\nseti 10362650 3 5\nbani 2 255 4\naddr 5 4 5\nbani 5 16777215 5\nmuli 5 65899 5\nbani 5 16777215 5\ngtir 256 2 4\naddr 4 3 3\naddi 3 1 3\nseti 27 4 3\nseti 0 3 4\naddi 4 1 1\nmuli 1 256 1\ngtrr 1 2 1\naddr 1 3 3\naddi 3 1 3\nseti 25 2 3\naddi 4 1 4\nseti 17 7 3\nsetr 4 0 2\nseti 7 8 3\neqrr 5 0 4\naddr 4 3 3\nseti 5 1 3\n"
