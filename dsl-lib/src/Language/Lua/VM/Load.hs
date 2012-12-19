module Language.Lua.VM.Load
where

import Control.Applicative      ( (<$>) )

import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

import Language.Lua.Token	     ( tokenize )
import Language.Lua.Parser	     ( parse_chunk )
import Language.Lua.GenCode	     ( gencode_chunk )
import Language.Lua.GenCode.Assemble ( assembleProg )

-- import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Value
import Language.Lua.VM.Core
import Language.Lua.VM.CoreFct

-- ------------------------------------------------------------

addLoadFunctions :: LuaAction ()
addLoadFunctions
    = theGlobalEnv >>= addFunctions loadFcts >> return ()

loadFcts :: [(String, NativeAction)]
loadFcts
    = [ ( "loadstring"
        , oneOrMoreArgs "loadstring"
          >=> checkArgs "loadString" [check1 checkString]
          >=> loadString
        )
      ]

-- ------------------------------------------------------------

-- load, loadfile and relatives can be implemented with loadString

-- processes args with compileAndLoad
-- in case of success, the closure is called with an empty arg list
-- else the error res is propagated

loadString :: NativeAction
loadString vs
    = do res <- compileAndLoad vs
         case res of
           (C cls) -> do callClosure cls	-- modify env, pc and call stack for exec of closure with next instr
                         return emptyTuple	-- push the empty tuple onto the eval stack
           _       -> return res		-- push the error res onto the eval stack


-- | compiles the source given in the 1.arg (String)
-- success: result is a closure for executing the chunk of code
-- error: result is a tuple: (nil, error message)

compileAndLoad :: NativeAction
compileAndLoad [v]
    = compileAndLoad [v, v]
compileAndLoad (vs : vmsg0 : _)
    = do src     <-  value2str <$> checkString vs
         srcName <- (value2str <$> checkString vmsg0) `orElse` return src
         ( do code  <- compile src srcName
              start <- loadCode code
              env   <- gets theCurrEnv
              return $ C $ CL { theClosureEnv = Env [last (theEnv env)]
                              , theCodeAddr   = start
                              }
           ) `catchError` comperr
    where
      comperr :: String -> LuaAction Value
      comperr e
          = return $ tuple2Value [nil, S e]
          
compileAndLoad []
    = error "compileAndLoad: illegal argument []"

orElse :: LuaAction v -> LuaAction v -> LuaAction v
orElse a1 a2
    = a1 `catchError` const a2

-- ------------------------------------------------------------

compile :: String -> String -> LuaAction LuaModule
compile src srcName
    = (scan >=> parse >=> gen >=> assemble) src
    where
      scan
          = either (throwError . ("lexical error in program: " ++) . show) return
            . tokenize srcName

      parse
          = either (throwError . ("syntax error in program: " ++) . show) return
            . parse_chunk srcName

      gen
          = either (throwError . ("semantic error(s) in program: " ++)) return
            . gencode_chunk srcName

      assemble
          = return . assembleProg

type Process = ErrorT String IO

runProcess :: (arg -> Process res) -> arg -> IO (Either String res)
runProcess p inp = runErrorT (p inp)

-- ------------------------------------------------------------
