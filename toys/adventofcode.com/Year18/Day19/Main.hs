{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}
-- solution for
-- http://adventofcode.com/2018/day/19

module Main where

import           Data.Bits       ((.&.), (.|.))
import           Data.Array.IArray
-- import           Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
import           Data.List       (foldl', intercalate, intersect, delete)
import           Util.Main1 (main12)

import           Control.Arrow (second)
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.Reader

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char

import Debug.Trace

-- ----------------------------------------

main :: IO ()
main = do
  main12 "2018-19"
    inp captcha1
    inp captcha2

captcha1 :: String -> String
captcha1 = show . solve1 . fromString

captcha2 :: String -> String
captcha2 = show . solve2 . fromString

-- ----------------------------------------

withTrace :: Bool
withTrace = False

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
           }
           deriving (Show, Eq)

type Val       = Int

toMem :: [Val] -> Int -> Mem
toMem ms i = Mem
             { mem = ms
             , ic = i
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

-- ----------------------------------------
--
-- the program

type Prog      = Array Int MInstr
type MInstr    = (OpCode, (Int, Int, Int))
type OpCode    = Int

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

type Action = ExceptT Terminated (ReaderT Prog (State Mem))

data Terminated = ICoutOfRange deriving Show

type Instr = Int -> Int -> Int -> Action ()

runMachine :: Prog -> ([Val], Int) -> (Either Terminated (), [Val])
runMachine prog m0 =
  second mem $ runState (runReaderT (runExceptT runProg) prog) (uncurry toMem m0)

runProg :: Action ()
runProg = forever exec1Instr

exec1Instr :: Action ()
exec1Instr = do
  m0 <- get                          -- for trace
  mi@(oc, (x1, x2, x3)) <- getIns
  let f = snd (allInstr !! oc)
  f x1 x2 x3

  m1 <- get                          -- for trace
  trace' (showMem m0 ++ " " ++ showMInstr mi ++ " " ++ showVals (mem m1)) $ return ()
  incrIC

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

allInstr :: [(Int, Instr)]
allInstr = zip [0..]
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

solve1 :: Input -> Int
solve1 (bindIC, is) =
  head . snd . runMachine (toProg is) $ (replicate 6 0, bindIC)

solve' (bindIC, is) =
  runMachine (toProg is) (replicate 6 0, bindIC)

solve2 :: Input -> Result2
solve2  = undefined

-- ----------------------------------------

showMInstr :: MInstr -> String
showMInstr (oc, (x1, x2, x3)) = unwords
  [ mnemonics !! oc, show x1, show x2, show x3]

showMem :: Mem -> String
showMem m =
  "ip=" ++ show (mem m !! ic m) ++ " " ++ showVals (mem m)

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

    opcode :: SP OpCode
    opcode = foldr1 (<|>) $
      zipWith toOc [0..] mnemonics

    toOc :: OpCode -> String -> SP OpCode
    toOc i s = try (string s) *> return i

    nl = char '\n'
    sp = char ' '

mnemonics :: [String]
mnemonics =
  [ "addr"
  , "addi"
  , "mulr"
  , "muli"
  , "banr"
  , "bani"
  , "borr"
  , "bori"
  , "setr"
  , "seti"
  , "gtir"
  , "gtri"
  , "gtrr"
  , "eqir"
  , "eqri"
  , "eqrr"
  ]

-- ----------------------------------------

ex1 :: String
ex1 = unlines
  [ "#ip 0"
  , "seti 5 0 1"
  , "seti 6 0 2"
  , "addi 0 1 0"
  , "addr 1 2 3"
  , "setr 1 0 0"
  , "seti 8 0 4"
  , "seti 9 0 5"
  ]

inp' :: IO String
inp' = readFile "Year18/Day19/day19.txt"

inp :: String
inp = "#ip 5\naddi 5 16 5\nseti 1 8 2\nseti 1 1 1\nmulr 2 1 4\neqrr 4 3 4\naddr 4 5 5\naddi 5 1 5\naddr 2 0 0\naddi 1 1 1\ngtrr 1 3 4\naddr 5 4 5\nseti 2 8 5\naddi 2 1 2\ngtrr 2 3 4\naddr 4 5 5\nseti 1 7 5\nmulr 5 5 5\naddi 3 2 3\nmulr 3 3 3\nmulr 5 3 3\nmuli 3 11 3\naddi 4 6 4\nmulr 4 5 4\naddi 4 5 4\naddr 3 4 3\naddr 5 0 5\nseti 0 0 5\nsetr 5 3 4\nmulr 4 5 4\naddr 5 4 4\nmulr 5 4 4\nmuli 4 14 4\nmulr 4 5 4\naddr 3 4 3\nseti 0 3 0\nseti 0 0 5\n"
