{-# LANGUAGE ExistentialQuantification #-}

module Language.Lua.Value
where

import Control.Monad.Trans

import Data.Function        ( on )
import Data.IORef
import Data.Map             ( Map )
import Data.Maybe           ( fromJust )
import Data.Typeable
import Data.Unique

import qualified Data.Map as M

-- ------------------------------------------------------------
--
-- a Lua value is one of the 4 basic values, a table or a function value
-- (a closure)

data Value
    = Nil
    | B Bool
    | S String
    | N Double
    | T Table
    | C Closure
    | U UserData
      deriving (Eq, Ord)


-- function calls and rhs of expressions compute a list of values

type Values
    = [Value]

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

isNil         :: Value -> Bool
isNil Nil     = True
isNil _       = False

luaType       :: Value -> String
luaType  Nil  = "nil"
luaType (B _) = "boolean"
luaType (S _) = "string"
luaType (N _) = "number"
luaType (T _) = "table"
luaType (C _) = "function"
luaType (U _) = "userdata"

-- ------------------------------------------------------------

-- a table is (Lua language def) an object, every table constructor
-- generates a unique key, the TableId, used for comparison of tables
-- when assigning table values, a ref to the table is copied, not the
-- entries. This is the reason for the IORef.

data Table
    = TB { theTableId :: TableId
         , theTableEntries :: IORef Entries
         }

newtype TableId
    = TID Unique
      deriving (Eq, Ord)

type Entries
    = Map Value Cell

type Cell = IORef Value

instance Eq Table where
    (==) = (==) `on` theTableId

instance Ord Table where
    compare = compare `on` theTableId

newTable :: (MonadIO m) => m Table
newTable
    = liftIO $
      do i <- newUnique
         t <- newIORef M.empty
         return $ TB { theTableId = TID i, theTableEntries = t }

lookupEntry :: (MonadIO m) => Value -> Table -> m Value
lookupEntry k t
    = liftIO $
      do entries <- readIORef (theTableEntries t)
         maybe (return nil) (readIORef) $ M.lookup k entries

updateEntry :: (MonadIO m) => Value -> Value -> Table -> m Table
updateEntry k v t
    = liftIO $
      do entries <- readIORef (theTableEntries t)
         case M.lookup k entries of
           Nothing -> do c <- newIORef v
                         let entries' = M.insert k c entries
                         writeIORef (theTableEntries t) entries'
           Just c  -> writeIORef c v
         return t

removeEntry :: MonadIO m => Value -> Table -> m Table
removeEntry k t
    = liftIO $
      do entries <- readIORef (theTableEntries t)
         case M.lookup k entries of
           Nothing -> return ()
           Just _  -> do let entries' = M.delete k entries
                         writeIORef (theTableEntries t) entries'
         return t

addEntry :: MonadIO m => Value -> Value -> Table -> m Table
addEntry k v t
    | isNil k
        = error "table index is nil"
    | isNil v
        = removeEntry k t
    | otherwise
        = updateEntry k v t

-- ------------------------------------------------------------

-- a closure consists of a table for all none local and local variables
-- closures can be identified by their environments, the envs have a unique id

type Env = Table

data Closure
    = CL { theEnv  :: Env
         , theCode :: Env -> Values -> Values
         }

instance Eq Closure where
    (==) = (==) `on` theEnv

instance Ord Closure where
    compare = compare `on` theEnv


-- ------------------------------------------------------------

-- a program state is a mapping from module names to tables
-- a single table contains all global variables of a module
-- there is one anonymous module for all global variables
-- and (predefined) functions

newtype State
    = ST { theModules :: Table
         }

-- ------------------------------------------------------------




