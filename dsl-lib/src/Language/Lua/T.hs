module T
where

import Language.Lua.Parser
import Language.Lua.Token
import Language.Lua.AST

import Control.Applicative ( (<$>) )
import Control.Monad.RWS

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
    = Add | Sub | Mult | Div | Exp | Mod | Conc | EQ | NEQ | GR | GE | LE | LT | Pack
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
    | LoadField
    | NewTable
    | NewEnv
    | StoreVar Name
    | StoreLocal Name
    | StoreField
    | Pop
    | BinOp BOp
    | UnOp UOp
    | Jump lab
    | Label lab
    | Branch Bool lab
    | Closure lab
    | Call
    | TailCall
      deriving (Show)

-- ------------------------------------------------------------

newtype Label = Lab Int
instance Show Label where
    show (Lab l) = format l
        where
          format = ("L" ++) . reverse . take 5 . reverse . ("0000" ++) . show
               
type AInstr = Instr Label
type ACode = [AInstr]

type MInstr = Instr Int
type MCode = [MInstr]

-- ------------------------------------------------------------

type Compile = RWS CEnv CErrs CState

type CEnv = ()

type CErrs
    = [CErr]
type CErr
    = String

data CState
    = CS { newLab :: Int }
      deriving (Show)

initCEnv :: ()
initCEnv = ()

initCState :: CState
initCState = CS { newLab = 1 }

compileProg :: Block -> (ACode, CState, CErrs)
compileProg block = runRWS (compBlock block) initCEnv initCState

-- ------------------------------------------------------------

compBlock :: Block -> Compile ACode
compBlock (Block sl)
    = concat <$> mapM compStmt sl

compStmt :: Stmt -> Compile ACode
compStmt (SAssignment lvs es)
    = do c1 <- compExprL es
         c2 <- compStore lvs
         return $ c1 ++ c2

compStmt s
    = do todo s
         return []

-- ------------------------------------------------------------

compExprL :: [Expr] -> Compile ACode
compExprL es
    = do todo es
         return [LoadVar "???"]

compStore :: [LValue] -> Compile ACode
compStore lvs
    = do todo lvs
         return [StoreVar "???"]

-- ------------------------------------------------------------
--
-- mothers little helpers

todo :: (Show a) => a -> Compile ()
todo ast = tell (["TODO: " ++ show ast])

newLabel :: Compile Label
newLabel
    = do s <- get
         let l = newLab s
         put $ s { newLab = l + 1 }
         return $ Lab l
      
-- ------------------------------------------------------------
