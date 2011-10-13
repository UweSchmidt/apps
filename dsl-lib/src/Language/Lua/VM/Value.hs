{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS -fno-warn-orphans #-}

module Language.Lua.VM.Value
where

import Control.Applicative 	( (<$>) )

import Control.Monad.Trans      ( MonadIO
                                , liftIO
                                )

import Data.Array.IArray	( listArray )
import Data.IORef		( newIORef
                                , readIORef
                                , writeIORef
                                , modifyIORef
                                )
import Data.List            	( intercalate )
import Data.Maybe           	( fromMaybe )
import Data.Unique          	( newUnique
                                , hashUnique
                                )

import Language.Lua.VM.Types

import System.IO		( hPutStrLn
                                , stderr
                                )

import qualified Data.Map as M

-- ------------------------------------------------------------

instance Show Value where
    show = value2String
           where
             value2String  :: Value -> String
             value2String Nil       = "nil"
             value2String (B True)  = "true"
             value2String (B False) = "false"
             value2String (N d)     = let (n, f) = properFraction d in
                                      if f == 0
                                      then show (n::Integer)
                                      else show d
             value2String (S s)     = show s
             value2String (T t)     = "table: "    ++ ( show . hashUnique . theTID . theTableId   $ t)
             value2String (C c)     = "function: " ++ ( show . theCodeAddr $ c)
             value2String (F f)     = "native function: " ++ ( show . theNativeFctId $ f)
             value2String (U _)     = "<userdata>"
             value2String (L l)     = ("{" ++) . (++ "}") . intercalate "," . map value2String $ l

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
luaType (F _) = "function"
luaType (U _) = "userdata"
luaType (L _) = "list"		-- should not be visible when evaluating any expressions

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

lengthTable :: (MonadIO m) => Table -> m Value
lengthTable t
    = do es <- getEntries t
         return . lengthEntries $ es

appendTable :: (MonadIO m) => Value -> Table -> m ()
appendTable v t
    = modifyEntries t (appendEntry v)

-- ------------------------------------------------------------

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

lengthEntries :: Entries -> Value
lengthEntries e
    = N . fromIntegral . len $ 0
      where
        em = theEntries e
        len :: Int -> Int
        len n
            = maybe n (const $ len n1) $ M.lookup (N . fromIntegral $ n1) em
              where
                n1 = n + 1

appendEntry :: Value -> Entries -> Entries
appendEntry (L vs) es
    = foldl (\ es' (v, k) -> writeEntry (N k) v es') es ps
      where
        (N d)  = lengthEntries es
        ps     = zip vs . map (+ d) $ [0..]

appendEntry v es
    = writeEntry (lengthEntries es) v es

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

emptyEnv :: Env
emptyEnv = Env { theEnv = [] }

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

newClosure :: Env -> CodeAddress -> Closure
newClosure e start
    = CL { theClosureEnv  = e
         , theCodeAddr    = start
         }

-- ------------------------------------------------------------

newNativeFct :: String -> NativeAction -> NativeFct
newNativeFct = NF

-- ------------------------------------------------------------

emptyLuaState :: LuaState
emptyLuaState
    = LuaState
      { theCurrEnv    = emptyEnv
      , thePC         = CA 0
      , theIntReg     = Nothing
      , theEvalStack  = []
      , theCallStack  = []
      , theProg       = listArray (0,(-1)) []
      , theLogger     = \ s -> liftIO (hPutStrLn stderr s)
--    , theLogger     = \ _ -> return ()
      }

-- ------------------------------------------------------------

emptyLuaEnv :: LuaEnv
emptyLuaEnv
    = LuaEnv { }

-- ------------------------------------------------------------
