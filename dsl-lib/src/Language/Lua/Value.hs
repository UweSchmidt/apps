{-# LANGUAGE ExistentialQuantification #-}

module Language.Lua.Value
where

import Control.Applicative ( (<$>) )
import Control.Monad.Trans

import Data.Function        ( on )
import Data.IORef
import Data.List            ( intercalate )
import Data.Map             ( Map )
import Data.Maybe           ( fromJust
                            , fromMaybe
                            )
import Data.Typeable
import Data.Unique

import qualified Data.Map as M

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
    | U UserData
    | L Values		-- lists of values are used internally with the ... varargs "variable"
                        -- lists should not be nested
      deriving (Eq, Ord)

instance Show Value where
    show = value2String

-- ------------------------------------------------------------

-- function calls and rhs of expressions compute a list of values

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

nil           :: Value
nil           = Nil

true          :: Value
true          = B True

false         :: Value
false         = B False

emptyList     :: Value
emptyList     = L []

isNil         :: Value -> Bool
isNil Nil     = True
isNil x@(L _) = isNil (list2Value x)
isNil _       = False

isFalse	          :: Value -> Bool
isFalse (B False) = True
isFalse x@(L _)   = isFalse (list2Value x)
isFalse x         = isNil x

isTrue            :: Value -> Bool
isTrue            = not . isFalse

-- ------------------------------------------------------------

luaType       :: Value -> String
luaType  Nil  = "nil"
luaType (B _) = "boolean"
luaType (S _) = "string"
luaType (N _) = "number"
luaType (T _) = "table"
luaType (C _) = "function"
luaType (U _) = "userdata"
luaType (L _) = "list"		-- should not be visible when evaluating any expressions

-- ------------------------------------------------------------

value2String  :: Value -> String
value2String Nil = "nil"
value2String (B True) = "true"
value2String (B False) = "false"
value2String (N d) = let (n, f) = properFraction d in
                     if f == 0
                     then show (n::Integer)
                     else show d
value2String (S s) = show s
value2String (T t) = "table: "    ++ ( show . hashUnique . theTID . theTableId   $ t)
value2String (C c) = "function: " ++ ( show . hashUnique . theCID . theClosureId $ c)
value2String (U _) = "<userdata>"
value2String (L l) = ("{" ++) . (++ "}") . intercalate "," . map value2String $ l

-- ------------------------------------------------------------

value2Int      :: Value -> Maybe Int
value2Int      = value2Integral

value2Integer  :: Value -> Maybe Integer
value2Integer  = value2Integral

value2Integral :: Integral a => Value -> Maybe a
value2Integral (N d)
    | f == 0.0
        = Just n
    | otherwise
        = Nothing
    where
      (n, f) = properFraction d
value2Integral _
    = Nothing

-- ------------------------------------------------------------

-- conversion of an arbitrary list of values to a single value

list2Value                 :: Value -> Value
list2Value (L [])          = nil		-- map empty tuple to nil
list2Value (L (x : _))     = x 	                -- map none empty list to head of list
list2Value v               = v

-- merging lists of values of right hand sides of assignments or returns
-- into a single list of values, the 1. arg is converted into a single value and cons'd to the second arg

consValues                 :: Value -> Value -> Value
consValues v1@(L _) v2     = consValues (list2Value v1) v2
consValues v1       (L []) = v1
consValues v1       (L l2) = L (v1 : l2)
consValues v1       v2     = L [v1,v2]

-- splitting a single value from a list value

uncons                     :: Value -> (Value, Value)
uncons (L [])              = (nil, emptyList)
uncons (L (x : xs))        = (x, L xs)
uncons v                   = (v, emptyList)

-- a multiple assignment can be implemented with these primitive functions
--
-- "x, y, z = f1(), f2()" can be implemented by
--
-- let v1 = eval("f1()")
--   , v2 = eval("f2()")
--   , v3 = consValues v1 v2
--   , (vx, v4) = uncons v3
--   , (vy, v5) = uncons v4
--   ,  vz      = list2Value v5
-- in
--   assign(x, vx)
--   assign(y, vy)
--   assign(z, vz)

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

-- ------------------------------------------------------------

newTable :: (MonadIO m) => m Table
newTable
    = liftIO $
      do i <- TID <$> newUnique
         t <- newIORef emptyEntries
         return $ TB { theTableId      = i
                     , theTableEntries = t
                     , theMetaTable    = nil
                     }

getEntries :: (MonadIO m) => Table -> m Entries
getEntries
    = liftIO . readIORef . theTableEntries

putEntries :: (MonadIO m) => Table -> Entries -> m ()
putEntries t entries
    = liftIO $ writeIORef (theTableEntries t) entries

modifyEntries :: (MonadIO m) => Table -> (Entries -> Entries) -> m ()
modifyEntries t mf
    = liftIO $ modifyIORef (theTableEntries t) mf

hasTableEntry :: (MonadIO m) => Value -> Table -> m Bool
hasTableEntry k t
    = do es <- getEntries t
         return (hasEntry k es)

readTable :: (MonadIO m) => Value -> Table -> m Value
readTable k t
    = do es <- getEntries t
         return . fromMaybe nil . lookupEntry k $ es

writeTable :: (MonadIO m) => Value -> Value -> Table -> m ()
writeTable k v t
    = modifyEntries t (writeEntry k v)

deleteTableEntry :: (MonadIO m) => Value -> Table -> m ()
deleteTableEntry k t
    = modifyEntries t (deleteEntry k)

-- ------------------------------------------------------------

newtype Entries
    = ET { theEntries :: Map Value Value}

emptyEntries :: Entries
emptyEntries
    = ET $ M.empty

hasEntry :: Value -> Entries -> Bool
hasEntry k
    = M.member k . theEntries

lookupEntry :: Value -> Entries -> Maybe Value
lookupEntry k
    = M.lookup k . theEntries

writeEntry :: Value -> Value -> Entries -> Entries
writeEntry k v
    = ET . M.insert k v . theEntries

deleteEntry :: Value -> Entries -> Entries
deleteEntry k
        = ET . M.delete k   . theEntries

-- ------------------------------------------------------------

-- the environment is organized as a stack of
-- tables, the topmost table contains the variable of the innermost scope
-- the last table holds the global variables
--
-- addNewEnv must be called when entering a new scope (function body or block)
-- when leaving the scope, the old list of envs must be restored
--
-- Hint for using these basic env ops:
--
-- a local variable ("local x") should perform a "newLocalVariable x" to store values
-- a "x = nil" must NOT perform a "deleteVariable x", the variable is still marked as local
-- a "x = someOtherValue" must perform a "writeVariable x ..."
--
-- in tables used as env, deleting variable is never done, because of the
-- static binding semantics of lua, all none global variable "deleted" must
-- remain in the table with the associated value "nil", else a kind of dynamic binding
-- would be the effect

newtype Env = Env { theEnv :: [Table] }

globalEnv :: Table -> Env
globalEnv gt = Env { theEnv = [gt] }

addNewEnv ::  (MonadIO m) => Env -> m Env
addNewEnv (Env e)
    = do t <- newTable
         return $ Env (t : e)

readVariable :: (MonadIO m) => Value -> Env -> m Value
readVariable k (Env [ge])
    = readTable k ge

readVariable k (Env (e1 : env))
    = do res <- readTable k e1
         if isNil res
            then readVariable k (Env env)
            else return res

readVariable _ _
    = error "readVariable: no env given"

newLocalVariable :: (MonadIO m) => Value -> Env -> m ()
newLocalVariable k e
    = writeLocalVariable k nil e

writeLocalVariable :: (MonadIO m) => Value -> Value -> Env -> m ()
writeLocalVariable k v (Env (e1 : _))
    = writeTable k v e1

writeLocalVariable _ _ _
    = error "writeLocalVariable: no env given"

writeVariable :: (MonadIO m) => Value -> Value -> Env -> m ()
writeVariable k v (Env [ge])
    = writeTable k v ge

writeVariable k v (Env (e1 : env))
    = do found <- hasTableEntry k e1
         if found
            then writeTable k v e1
            else writeVariable k v (Env env)

writeVariable _ _ _
    = error "writeVariable: no env given"

-- ------------------------------------------------------------

-- a closure consists of a list of tables, the static chain of environments
-- on call of this closure the env will be extended by a new empty env for
-- the parameters and locals

data Closure
    = CL { theClosueEnv  :: Env
         , theClosureId  :: ClosureId		-- just for comparisons of function values
         , theCode       :: Function
         }

type Function
    = Closure -> Values -> IO Values

newtype ClosureId
    = CID { theCID :: Unique }
      deriving (Eq, Ord)

instance Eq Closure where
    (==) = (==) `on` theClosureId

-- this instance is not very usefull
-- it's required by the Ord instance of Value

instance Ord Closure where
    compare = compare `on` (hashUnique . theCID . theClosureId)


newClosure :: (MonadIO m) => Env -> Function -> m Closure
newClosure e f
    = liftIO $
      do i <- CID <$> newUnique
         return $
                CL { theClosueEnv  = e
                   , theClosureId  = i
                   , theCode       = f
                   }

-- ------------------------------------------------------------

-- a program state is a mapping from module names to tables
-- a single table contains all global variables of a module
-- there is one anonymous module for all global variables
-- and (predefined) functions

newtype State
    = ST { theModules :: Table
         }

-- ------------------------------------------------------------

