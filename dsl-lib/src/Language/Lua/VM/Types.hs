{-# LANGUAGE ExistentialQuantification #-}

module Language.Lua.VM.Types
where

import Data.Array.IArray
import Data.Function        	( on )
import Data.IORef
import Data.Map             	( Map )
import Data.Maybe           	( fromJust )
import Data.Monoid
import Data.Typeable
import Data.Unique

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

-- ------------------------------------------------------------
--
-- a Lua value is one of the 4 basic values, a table or a function value
-- (a closure), a user data value (rather anything)

data Value
    = Nil
    | B Bool
    | S String
    | N Double
    | T Table
    | C Closure
    | F NativeFct
    | U UserData
    | P Values		-- tuple of values are used internally with the ... varargs "variable"
                        -- tuples must not be nested
    deriving (Eq, Ord)

-- ------------------------------------------------------------

-- function calls and rhs of expressions compute a tuple of values

type Values
    = [Value]

-- ------------------------------------------------------------

newtype Err
    = Err { theErr :: String }
      deriving(Eq, Ord)

-- ------------------------------------------------------------

data UserData = forall a . (Typeable a, Eq a, Ord a) => UserData a

instance Eq UserData where
    (UserData x) == (UserData y) =
        typeOf x == typeOf y
        &&
        cast x == Just y

-- this Ord instance is a bit silly,
-- but it is required to have an Ord instance on Value
-- if the user data is not of the same type,
-- an artificial ordering is invented

instance Ord UserData where
    (UserData x) <= (UserData y)
        | typeOf x == typeOf y
            = fromJust (cast x) <= y
        | otherwise
            = show (typeOf x) <= show (typeOf y)

-- ------------------------------------------------------------

-- a table is (Lua language def) an object, every table constructor
-- generates a unique key, the TableId, used for comparison of tables
-- when assigning table values, a ref to the table is copied, not the
-- entries. This is the reason for the IORef.

data Table
    = TB { theTableId      :: TableId
         , theTableEntries :: IORef Entries
         , theMetaTable    :: Value
         }

instance Eq Table where
    (==) = (==) `on` theTableId

instance Ord Table where
    compare = compare `on` theTableId

newtype TableId
    = TID { theTID :: Unique }
      deriving (Eq, Ord)

newtype Entries
    = ET { theEntries :: Map Value Value}

newtype Env
    = Env { theEnv :: [Table] }

-- ------------------------------------------------------------

-- a closure consists of a list of tables, the static chain of environments
-- an a start address.
-- On call of this closure the env will be extended by a new empty env for
-- the parameters and locals

data Closure
    = CL { theClosureEnv  :: Env
         , theCodeAddr    :: CodeAddress
         }

newtype CodeAddress
    = CA { theCA :: Int }
      deriving (Eq, Ord, Show)

instance Eq Closure where
    (==) = (==) `on` theCodeAddr

instance Ord Closure where
    compare = compare `on` theCodeAddr

-- ------------------------------------------------------------

-- A native function is an action transforming the Lua state
-- This definition leads to recursive dependencies between
-- Value(s), LuaState(s) and Instr(uctions) and requires to put all semantic domains
-- into a single module

type NativeAction
    = Values -> LuaAction Value

data NativeFct
    = NF { theNativeFctId :: String
         , theNativeFct   :: NativeAction
         }

instance Eq NativeFct where
    (==) = (==) `on` theNativeFctId

instance Ord NativeFct where
    compare = compare `on` theNativeFctId

-- ------------------------------------------------------------

-- a program state is a mapping from module names to tables
-- a single table contains all global variables of a module
-- there is one anonymous module for all global variables
-- and (predefined) functions

data LuaState
    = LuaState
      { theCurrEnv    :: Env
      , thePC         :: CodeAddress
      , theIntrReg    :: Maybe LuaError
      , theEvalStack  :: Values
      , theCallStack  :: [Closure]
      , theProg       :: LuaCode	-- for dynamic loading the code is part of the state, else the prog could be a part of the env
      , theLogger     :: LuaLogger
      }

-- ------------------------------------------------------------

data LuaLogger
    = LuaLogger
      { logCmd     :: String -> LuaAction ()
      , instrLog   :: Bool
      , evalLog    :: Bool
      }

-- ------------------------------------------------------------

data LuaEnv
    = LuaEnv { }

-- ------------------------------------------------------------

type LuaError   = String

type LuaModule  = MCode

type LuaCode    = [((Int, Int), Array Int Instr)]

type LuaAction  = Eval LuaError LuaEnv LuaState

-- ------------------------------------------------------------
--
-- | Evaluation of an expression/command runs in an
-- error-reader-state-IO monad

type Eval err env state
    = ErrorT err (ReaderT env (StateT state IO))

runEval :: (Error err) =>
           Eval err env state res ->
           env -> state ->
           IO (Either err res, state)
runEval expr e0 s0 
    = runStateT (runReaderT (runErrorT expr) e0) s0

-- ------------------------------------------------------------

type VName
    = String

data BOp
    = Add | Sub | Mult | Div | Exp | Mod | EQU | NEQ | GRT | GRE | LSE | LST | Conc
      deriving (Show, Eq, Ord)

data UOp
    = Minus | Not | NumberOf
      deriving (Show, Eq, Ord)

data Instr
    = LoadNum Double
    | LoadStr String
    | LoadBool Bool
    | LoadNil
    | LoadEmpty		-- load empty result tuple
    | LoadVar VName
    | LoadField         -- top: the table, 2.: the index
    | NewTable
    | NewEnv            -- create a new empty env (on block or function entry)
    | DelEnv            -- remove an env on block exit (normal exit, break and return)
    | NewLocal VName    -- create new variable in local env
    | StoreVar VName
    | StoreField        -- top: the table, 2.: the index, 3. the value
    | AppendField       -- top: the table, 2.: the value(-list)
    | MkTuple		-- top: the tail (single value or list), 2. the head value
    | UnTuple           -- top: the head or nil, 2. the rest, maybe the empty tuple
    | Take1             -- top: take the head or nil, discard the rest
    | Pop               -- throw away the topmost value
    | Copy Int          -- push the i. value onto the stack, 0 == top of stack, stack growes by 1
    | Move Int          -- move the i. value to the top, i == 1: swap the 2 topmost values, stack remains size
    | BinOp BOp         -- top: right arg, 2.: left arg
    | UnOp UOp          -- top: single arg
    | Jump Dest
    | Label Label
    | Branch Bool Dest  -- pop top of stack and test on (false or nil)
    | Closure Dest      -- push a closure
    | Call              -- top: closure, 2. args
    | TailCall          -- top: closure, 2. args
    | Leave             -- remove current env and return to saved stored closure
    | Intr String       -- interrupt

-- ------------------------------------------------------------

data Dest
    = M Label
    | D Int

instance Show Dest where
    show (M l) = show l
    show (D d) = show d

-- ------------------------------------------------------------

newtype Label
    = Lab Int
      deriving (Eq, Ord)

instance Show Label where
    show (Lab l) = format l
        where
          format = ("l" ++) . reverse . take 3 . reverse . ("0000" ++) . show
               
-- ------------------------------------------------------------

newtype Code
    = Code [Instr]

instance Monoid Code where
    mempty = Code []
    mappend (Code c1) (Code c2) = Code $ c1 ++ c2

-- ------------------------------------------------------------

newtype MCode
    = MCode [Instr]

-- ------------------------------------------------------------


