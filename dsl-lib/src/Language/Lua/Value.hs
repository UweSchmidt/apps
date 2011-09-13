{-# LANGUAGE ExistentialQuantification #-}

module Language.Lua.Value
where

import Control.Applicative ( (<$>) )
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
-- (a closure), a user data value (rather anything)

data Value
    = Nil
    | B Bool
    | S String
    | N Double
    | T Table
    | C Closure
    | U UserData
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

isNil         :: Value -> Bool
isNil Nil     = True
isNil _       = False

-- ------------------------------------------------------------

luaType       :: Value -> String
luaType  Nil  = "nil"
luaType (B _) = "boolean"
luaType (S _) = "string"
luaType (N _) = "number"
luaType (T _) = "table"
luaType (C _) = "function"
luaType (U _) = "userdata"

-- ------------------------------------------------------------

value2String  :: Value -> String
value2String Nil = "nil"
value2String (B True) = "true"
value2String (B False) = "false"
value2String (N d) = let (n, f) = properFraction d in
                     if f == 0
                     then show (n::Integer)
                     else show d
value2String (S s) = s
value2String (T t) = "table: "    ++ ( show . hashUnique . theTID . theTableId   $ t)
value2String (C c) = "function: " ++ ( show . hashUnique . theCID . theClosureId $ c)
value2String (U _) = "<userdata>"

-- ------------------------------------------------------------

value2Int :: Value -> Maybe Int
value2Int = value2Integral

value2Integer :: Value -> Maybe Integer
value2Integer = value2Integral

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

-- a table is (Lua language def) an object, every table constructor
-- generates a unique key, the TableId, used for comparison of tables
-- when assigning table values, a ref to the table is copied, not the
-- entries. This is the reason for the IORef.

data Table
    = TB { theTableId      :: TableId
         , theTableEntries :: IORef Variables
         , theMetaTable    :: Value
         }

newtype TableId
    = TID { theTID :: Unique }
      deriving (Eq, Ord)

instance Eq Table where
    (==) = (==) `on` theTableId

instance Ord Table where
    compare = compare `on` theTableId

-- ------------------------------------------------------------

newTable :: (MonadIO m) => m Table
newTable
    = liftIO $
      do i <- newUnique
         t <- newIORef M.empty
         return $ TB { theTableId      = TID i
                     , theTableEntries = t
                     , theMetaTable    = nil
                     }

getEntries :: (MonadIO m) => Table -> m Variables
getEntries
    = liftIO . readIORef . theTableEntries

putEntries :: (MonadIO m) => Table -> Variables -> m ()
putEntries t entries
    = liftIO $ writeIORef (theTableEntries t) entries

lookupTable :: (MonadIO m) => Value -> Table -> m Value
lookupTable k t
    = do entries <- getEntries t
         lookupVariable k entries

writeTable :: (MonadIO m) => Value -> Value -> Table -> m Table
writeTable k v t
    = do entries <- getEntries t
         res <- writeVariable k v entries
         maybe (return ()) (putEntries t) res
         return t
{-
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
        = writeTable k v t
-- -}
-- ------------------------------------------------------------

type Variables
    = Map Value Cell

type Cell = IORef Value

lookupVariable :: (MonadIO m) => Value -> Variables -> m Value
lookupVariable k vars
    = liftIO $
      maybe (return nil) (readIORef) $
      M.lookup k vars

removeVariable :: MonadIO m => Value -> Variables -> m (Maybe Variables)
removeVariable k vars
    = liftIO $
      return $
      if M.member k vars
         then Just $ M.delete k vars
         else Nothing

writeVariable :: (MonadIO m) => Value -> Value -> Variables -> m (Maybe Variables)
writeVariable k v vars
    = liftIO $
      case M.lookup k vars of
        Nothing -> do c <- newIORef v
                      return . Just $ M.insert k c vars
        Just c  -> do writeIORef c v
                      return Nothing

updateVariable :: MonadIO m => Value -> Value -> Variables -> m (Maybe Variables)
updateVariable k v t
    | isNil v
        = removeVariable k t
    | otherwise
        = writeVariable k v t

-- ------------------------------------------------------------

-- a closure consists of a list of tables, the static chain of environments

type Env = Table

data Closure
    = CL { theGlobEnv        :: Table
         , theExternalLocals :: Variables
         , theCode           :: Closure -> Values -> IO Values
         , theClosureId      :: ClosureId		-- just for comparisons of function values
         }

newtype ClosureId
    = CID { theCID :: Unique }
      deriving (Eq, Ord)

instance Eq Closure where
    (==) = (==) `on` theClosureId

-- this instance is not very usefull
-- it's required by the Ord instance of Value

instance Ord Closure where
    compare = compare `on` (hashUnique . theCID . theClosureId)


-- ------------------------------------------------------------

-- a program state is a mapping from module names to tables
-- a single table contains all global variables of a module
-- there is one anonymous module for all global variables
-- and (predefined) functions

newtype State
    = ST { theModules :: Table
         }

-- ------------------------------------------------------------




