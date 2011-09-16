module T
where

import Language.Lua.Parser
import Language.Lua.Token
import Language.Lua.AST

import Control.Applicative ( (<$>) )
import Control.Monad.RWS

import Data.Char           ( toLower )

-- ------------------------------------------------------------

t :: String -> Either String (ACode, CState, CErrs)
t inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b
                     -> Right $ compileProg b
{-

      = gencode . parse . scan $ inp
    where
      scan = tokenize ""

      parse (Left err) = Left . show $ err
      parse (Right ts) = gencode . parse_chunk "" $ ts

      gencode (Left err) = Left . show $ err
      gencode (Right b)  = Right . compileProg $ b
-}
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
    show (Branch b l  ) = fmt1 ("br" ++ if b then "true" else "false") (show l)
    show (Closure _l  ) = fmt1 "new" "{closue}"
    show (Call        ) = fmt0 "call"
    show (TailCall    ) = fmt0 "tailcall"
    show (Leave       ) = fmt0 "return"
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
         , loopEnvLevels :: [(Int, Label)]
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
    | hasLocalDefs sl
        = do incrEnvCnt
             body <- compStmtL sl
             decrEnvCnt
             genCode [ mkInstr NewEnv
                     , body
                     , mkInstr DelEnv
                     ]
    | otherwise			-- optimization: unnecessay env creation thrown away
        = compStmtL sl

compStmtL :: [Stmt] -> Compile ACode
compStmtL sl
    = mconcat <$> mapM compStmt sl

-- ------------------------------------------------------------

compStmt :: Stmt -> Compile ACode
compStmt (SAssignment [lv] [e])
    = do c1 <- compExpr1 e
         c2 <- compStore mempty lv
         genCode [c1, c2]

compStmt (SAssignment lvs es)
    = do c1 <- compExprL  es
         c2 <- compStoreL lvs
         genCode [c1, c2]

compStmt (SWhile cond body)
    = do lBody <- newLabel
         lCond <- newLabel
         lEnd  <- newLabel
         pushLoopLevel lEnd
         codeCond <- compCond True lBody cond
         codeBody <- compBlock body
         genCode [ mkInstr $ Jump  lCond
                 , mkInstr $ Label lBody
                 , codeBody
                 , mkInstr $ Label lCond
                 , codeCond
                 , mkInstr $ Label lEnd
                 ]

compStmt SBreak
    = getLoopLevel >>=
      maybe (do cerr "compiling break statement" "no surrounding loop found"
                return mempty
            ) (\ (i, lab) -> genCode $ replicate i (mkInstr DelEnv)
                                       ++
                                       [mkInstr $ Jump lab]
              )

compStmt (SReturn es)		-- TODO: Tail call optimization
    = do l        <- getEnvCnt
         code_es  <- compExprL es
         code_env <- genCode $ replicate l (mkInstr DelEnv)
         genCode [ code_es
                 , code_env
                 , mkInstr Leave
                 ]

compStmt s
    = do todo "compStmt" s
         return mempty

-- ------------------------------------------------------------

compStoreL :: [LValue] -> Compile ACode
compStoreL []
    = genCode [ mkInstr Pop ]

compStoreL lvs
    = do code_lvs' <- mapM compStore1 lvs'
         code_lv   <- compStoreLast $ head lv
         genCode $ code_lvs'
                   ++
                   [ code_lv ]
  where
    len1 = length lvs - 1
    (lvs', lv) = splitAt len1 lvs

compStore1 :: LValue -> Compile ACode
compStore1
    = compStore (mkInstr UnTuple)

compStoreLast :: LValue -> Compile ACode
compStoreLast
    = compStore (mkInstr Take1)

-- gen code for storing a single value from the eval stack into a variable
-- into a field of a table, in the 1 case stack size is decreased by 1
-- in the 2. case the top value is a ref to the table, the 2. the field and
-- the 3. the value, so the eval stack is decreased by 3

compStore :: ACode -> LValue -> Compile ACode
compStore instr (LVar n)
    = genCode [ instr
              , mkInstr $ StoreVar n
              ]

compStore instr (LFieldRef tb ix)
    = do code_ix <- compExpr1 ix
         code_tb <- compExpr1 tb
         genCode [ instr
                 , code_ix
                 , code_tb
                 , mkInstr StoreField
                 ]

-- ------------------------------------------------------------

-- gen code such that the list of expressions is pushed as a single list of values onto
-- the evaluation stack

compExprL :: [Expr] -> Compile ACode
compExprL []
    = return $ mkInstr $ LoadEmpty
compExprL es
    = do code_es' <- mapM compExpr1 es'
         code_le  <- compExpr (head le)
         genCode $ code_es'
                   ++
                   [ code_le ]
                   ++
                   replicate len1 (mkInstr MkTuple)
      where
        len1 = length es -1
        (es', le) = splitAt len1 es

-- gen code such that a single value, not a tuple is pushed onto the eval stack
-- from tuple values the 1. one (or nil) will be taken

compExpr1 :: Expr -> Compile ACode
compExpr1 e
    | isSingleResExpr e
        = compExpr e
    | otherwise
        = do code_e <- compExpr e
             genCode [ code_e
                     , mkInstr Take1
                     ]

-- gen code such that a single value is pushed onto the evaluation stack
-- this value may be a tuple of arbitrary length

compExpr :: Expr -> Compile ACode
compExpr (ENumber d)
    = genCode [mkInstr $ LoadNum d]

compExpr (EString s)
    = genCode [mkInstr $ LoadStr s]

compExpr (EBool b)
    = genCode [mkInstr $ LoadBool b]

compExpr (ENil)
    = genCode [mkInstr $ LoadNil]

compExpr (EVar n)
    = genCode [mkInstr $ LoadVar n]

compExpr e
    = do todo "compExpr" e
         return $ mkInstr $ Abort "compExpr"

-- gen code such that a single value is pushed onto the stack,
-- this value will be consumed by a conditional branch instr
-- so after executing the sequence, the stack has the same state as before

compCond :: Bool -> Label -> Expr -> Compile ACode
compCond c l e
    = do codeExpr <- compExpr e
         genCode [ codeExpr
                 , mkInstr $ Branch c l
                 ]

-- ------------------------------------------------------------
--
-- mothers little helpers

todo :: (Show a) => String -> a -> Compile ()
todo s ast = tell $ CErrs [unwords ["TODO:", s, show ast]]

cerr :: (Show a) => String -> a -> Compile ()
cerr s msg = tell $ CErrs [unwords ["ERROR", "in", s ++ ":", show msg]]

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

getEnvCnt :: Compile Int
getEnvCnt = envCnt <$> get

pushLoopLevel :: Label -> Compile ()
pushLoopLevel lab
    = modify $ \ s -> s { loopEnvLevels = (envCnt s, lab) : loopEnvLevels s }

popLoopLevel :: Compile ()
popLoopLevel
    = modify $ \ s -> s { loopEnvLevels = tail $ loopEnvLevels s }

getLoopLevel :: Compile (Maybe (Int, Label))
getLoopLevel
    = do s <- get
         return $
           case loopEnvLevels s of
             [] -> Nothing
             ((l, lab) : _) -> Just (envCnt s - l, lab)

-- ------------------------------------------------------------

isSingleResExpr :: Expr -> Bool
isSingleResExpr (EEllipsis)         = False
isSingleResExpr (ECall _ _)         = False
isSingleResExpr (EMemberCall _ _ _) = False
isSingleResExpr _                   = True

hasLocalDefs :: [Stmt] -> Bool
hasLocalDefs = any isLocalDef

isLocalDef :: Stmt -> Bool
isLocalDef (SLocalDef _ _) = True
isLocalDef _               = False

-- ------------------------------------------------------------
