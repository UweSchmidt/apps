module T
where

import Language.Lua.Parser
import Language.Lua.Token
import Language.Lua.AST

import Control.Applicative ( (<$>) )
import Control.Monad.RWS

import Data.Char           ( toLower )

-- ------------------------------------------------------------

t :: String -> Either String Block
t inp
      = gencode . parse . scan $ inp
    where
      scan = tokenize ""

      parse (Left err) = Left . show $ err
      parse (Right ts) = gencode . parse_chunk "" $ ts

      gencode (Left err) = Left . show $ err
      gencode (Right b)  = Right b

-- ------------------------------------------------------------

data BOp
    = Add | Sub | Mult | Div | Exp | Mod | EQ | NEQ | GR | GE | LE | LT
      deriving (Show, Eq, Ord)

data UOp
    = Minus | Not | NumberOf | Split
      deriving (Show, Eq, Ord)

data Instr lab
    = LoadNum Double
    | LoadStr String
    | LoadBool Bool
    | LoadNil
    | LoadEmpty		-- load empty result list
    | LoadVar Name
    | LoadField         -- binary op
    | NewTable
    | NewEnv            -- create a new empty env (on block or function entry)
    | DelEnv            -- remove an env on block exit (normal exit, break and return)
    | NewLocal Name     -- create new variable in local env
    | StoreVar Name
    | StoreField
    | MkTuple		-- cons the 2. top value with the tol value list
    | UnTuple           -- split top level value list into head and tail
    | Take1             -- take the head ov a value list and discard the tail
    | Pop               -- throw away the topmost value
    | BinOp BOp
    | UnOp UOp
    | Jump lab
    | Label lab
    | Branch Bool lab
    | Closure lab
    | Call
    | TailCall
    | Leave
    | Exit Int
    | Abort String	-- for debugging

instance (Show lab) => Show (Instr lab) where
    show (LoadNum d   ) = fmt1 "load" (show d)
    show (LoadStr s   ) = fmt1 "load" (show s)
    show (LoadBool b  ) = fmt1 "load" (if b then "true" else "false")
    show (LoadNil     ) = fmt1 "load" "nil"
    show (LoadEmpty   ) = fmt1 "load" "()"
    show (LoadVar n   ) = fmt1 "load" n
    show (LoadField   ) = fmt1 "load" ".[.]"
    show (NewTable    ) = fmt1 "new" "{}"
    show (NewEnv      ) = fmt1 "new" "{env}"
    show (NewLocal n  ) = fmt1 "new" n
    show (DelEnv      ) = fmt1 "del" "{env}"
    show (StoreVar n  ) = fmt1 "store" n
    show (StoreField  ) = fmt1 "store" ".[.]"
    show (MkTuple     ) = fmt0 "mktuple"
    show (UnTuple     ) = fmt0 "untuple"
    show (Take1       ) = fmt0 "take1"
    show (Pop         ) = fmt0 "pop"
    show (BinOp op    ) = fmt0 $ fmtOp $ show op
    show (UnOp op     ) = fmt0 $ fmtOp $ show op
    show (Jump l      ) = fmt1 "jump" (show l)
    show (Label l     ) = fmtL $ show l
    show (Branch b l  ) = fmt1 ("branch" ++ if b then "t" else "f") (show l)
    show (Closure _l  ) = fmt1 "new" "{closue}"
    show (Call        ) = fmt0 "call"
    show (TailCall    ) = fmt0 "tailcall"
    show (Leave       ) = fmt0 "leave"
    show (Exit rc     ) = fmt1 "exit" (show rc)
    show (Abort s     ) = fmt1 "abort" s

indent     :: String -> String
indent s   = replicate 8 ' ' ++ s

fill       :: Int -> String -> String
fill n s   = take n (s ++ replicate n ' ')

fmt0       :: String -> String
fmt0 s     = indent s

fmt1       :: String -> String -> String
fmt1 s0 s1 = indent $ fill 8 s0 ++ s1

fmtL       :: String -> String
fmtL l     = l ++ ":"

fmtOp      :: String -> String
fmtOp      = map toLower

-- ------------------------------------------------------------

newtype Label = Lab Int
instance Show Label where
    show (Lab l) = format l
        where
          format = ("l" ++) . reverse . take 3 . reverse . ("0000" ++) . show
               
type AInstr = Instr Label
type ACode  = Code  Label

type MInstr = Instr Int
type MCode  = Code  Int

-- ------------------------------------------------------------

newtype Code a
    = Code [Instr a]

instance (Show a) => Show (Code a) where
    show (Code is) = "\n" ++ concatMap ((++ "\n") . show) is

instance Monoid (Code a) where
    mempty = Code []
    mappend (Code c1) (Code c2) = Code $ c1 ++ c2

mkInstr :: Instr a -> Code a
mkInstr = Code . (:[])

-- ------------------------------------------------------------

type Compile = RWS CEnv CErrs CState

type CEnv = ()

newtype CErrs
    = CErrs [CErr]

instance Show CErrs where
    show (CErrs es) = "\n" ++ unlines es

instance Monoid CErrs where
    mempty = CErrs []
    mappend (CErrs e1) (CErrs e2) = CErrs $ e1 ++ e2

type CErr
    = String

data CState
    = CS { newLab :: Int
         , envCnt :: Int
         , loopEnvLevels :: [Int]
         }
      deriving (Show)

initCEnv :: ()
initCEnv = ()

initCState :: CState
initCState = CS { newLab      = 1
                , envCnt      = 0
                , loopEnvLevels = []
                }

compileProg :: Block -> (ACode, CState, CErrs)
compileProg block = runRWS (compBlock block) initCEnv initCState

-- ------------------------------------------------------------

genCode :: [Code a] -> Compile (Code a)
genCode = return . mconcat

compProg :: Block -> Compile ACode
compProg block
    = do c <- compBlock block
         genCode [ c
                 , mkInstr $ Exit 0
                 ]

compBlock :: Block -> Compile ACode
compBlock (Block sl)
    = do body <- mconcat <$> mapM compStmt sl
         genCode [ mkInstr NewEnv
                 , body
                 , mkInstr DelEnv
                 ]

compStmt :: Stmt -> Compile ACode
compStmt (SAssignment lvs es)
    = do c1 <- compExprL es
         c2 <- compStore lvs
         genCode [c1, c2]

compStmt s
    = do todo "compStmt" s
         return mempty

-- ------------------------------------------------------------

compExprL :: [Expr] -> Compile ACode
compExprL es
    = do todo "compExprL" es
         return $ mkInstr $ Abort "compExprL"

compStore :: [LValue] -> Compile ACode
compStore lvs
    = do todo "compStore" lvs
         return $ mkInstr $ Abort "compStore"

-- ------------------------------------------------------------
--
-- mothers little helpers

todo :: (Show a) => String -> a -> Compile ()
todo s ast = tell $ CErrs [unwords ["TODO:", s, show ast]]

newLabel :: Compile Label
newLabel
    = do s <- get
         let l = newLab s
         put $ s { newLab = l + 1 }
         return $ Lab l

modifyEnvCnt :: (Int -> Int) -> Compile ()
modifyEnvCnt f
    = modify $ \ s -> s { envCnt = f $ envCnt s }

incrEnvCnt :: Compile ()
incrEnvCnt = modifyEnvCnt (+   1 )

decrEnvCnt :: Compile ()
decrEnvCnt = modifyEnvCnt (+ (-1))

pushLoopLevel :: Compile ()
pushLoopLevel
    = modify $ \ s -> s { loopEnvLevels = envCnt s : loopEnvLevels s }

popLoopLevel :: Compile ()
popLoopLevel
    = modify $ \ s -> s { loopEnvLevels = tail $ loopEnvLevels s }

getLoopLevel :: Compile Int
getLoopLevel
    = do s <- get
         return $ envCnt s - head (loopEnvLevels s)

-- ------------------------------------------------------------
