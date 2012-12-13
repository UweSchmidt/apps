module  Language.Lua.VM.CoreFct
where

import Control.Applicative ( (<$>) )

import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

-- import Data.Array.IArray
import Data.List                ( intercalate )

-- import Language.Common.Eval

import Language.Lua.VM.Core
import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Value

-- import System.IO                ( hPutStrLn
--                                 , stderr
--                                 )

-- ------------------------------------------------------------

addFunctions :: [(String, NativeAction)] -> Table -> LuaAction Table
addFunctions fcts t 
    = do sequence_ . map (uncurry $ addFct t) $ fcts
         return t
    where
      addFct tab name fct
          = writeTable (S name) (F $ newNativeFct name fct) tab

-- ------------------------------------------------------------

addVMFunctions :: LuaAction ()
addVMFunctions
    = do t <-  newTable >>= addFunctions vmFcts
         ge <- theGlobalEnv
         writeTable (S "vm") (T t) ge

vmFcts :: [(String, NativeAction)]
vmFcts
    = [ {-
        ( "traceOn",       wrapProc $ setLoggingOn  )
      , ( "traceOff",      wrapProc $ setLoggingOff )
      , -}
        ( "traceOn",       wrapProc $ iLog True  )
      , ( "traceOff",      wrapProc $ iLog False >> eLog False )
      , ( "evalTraceOn",   wrapProc $ eLog True  >> iLog True  )
      , ( "evalTraceOff",  wrapProc $ eLog False )
      , ( "dumpState",     wrapProc $ dumpState  )
      ]
    where
      iLog b = setLogging $ \ lg -> lg { instrLog = b }
      eLog b = setLogging $ \ lg -> lg {  evalLog = b }

      wrapProc prc
          = \ vs -> prc >> return (tuple2Value vs)

      dumpState
          = get
            >>= (liftIO . dumpLuaState)
            >>= logMsg (const True)

-- ------------------------------------------------------------

addCoreFunctions :: LuaAction ()
addCoreFunctions
    = theGlobalEnv >>= addFunctions coreFcts >> return ()

coreFcts :: [(String, NativeAction)]
coreFcts
    = [ ( "assert"
        , oneOrMoreArgs "assert"
          >=> firstArgIsTrue
          >=> tupleRes
        )
      , ( "next"
        , oneOrMoreArgs "next"
          >=> checkArgs "next" [check1 checkTable, anyArg]
          >=> nextKeyValue
          >=> tupleRes
        )
      , ( "print"
        , (liftIO . putStrLn . intercalate "\t" . map show')
          >=> noRes
        )
      , ( "tonumber"
        , oneOrMoreArgs "tonumber"
          >=> (return . maybe nil N . value2number . head)
        )
      , ( "tostring"
        , oneOrMoreArgs "tostring"
          >=> (return . S . show' . head)
        )
      , ( "type"
        , oneOrMoreArgs "type"
          >=> (return . S . luaType . head)
        )
      ]
    where
      show' (S s) = s
      show' v'    = show v'

      nextKeyValue vs
          = nextKeyTable key tab >>= checkKeyFound
               
          where
            (T tab) : key : _ = vs
            checkKeyFound []
                = luaError $ unwords ["invalid key to next", show key]
            checkKeyFound vs'
                = return vs'

-- ------------------------------------------------------------

checkArgs :: String -> [String -> Values -> LuaAction Values] -> Values -> LuaAction Values
checkArgs _ [] _
    = return []

checkArgs fn (_c1 : _cs) []
    = luaError $ unwords ["arguments missing to", fn]

checkArgs fn (c1 : cs) (v1 : vs)
    = do v1' <- c1 fn [v1]
         vs' <- checkArgs fn cs vs
         return $ v1' ++ vs'

check1 :: (Value -> LuaAction Value) -> String -> Values -> LuaAction Values
check1 cf _fn vs
    = (:[]) <$> cf (head vs)

-- ------------------------------------------------------------

anyArg :: String -> Values -> LuaAction Values
anyArg _fn []
    = return [nil]
anyArg _ (v : _)
    = return [v]

oneOrMoreArgs :: String -> Values -> LuaAction Values
oneOrMoreArgs fn []
    =  luaError $ unwords ["bad argument #1 to", fn, "(value expected)"]
oneOrMoreArgs _ vs
    = return vs

firstArgIsTrue :: Values -> LuaAction Values
firstArgIsTrue vs
    | isFalse . head $ vs
        = luaError $ unwords ["assertion failed!"]
    | otherwise
        = return vs

-- ------------------------------------------------------------

noRes :: (res -> LuaAction Value)
noRes
    = const $ return emptyTuple

tupleRes :: Values -> LuaAction Value
tupleRes
    = return . tuple2Value

-- ------------------------------------------------------------
