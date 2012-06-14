module LuaTest
where

import Control.Arrow 			( first )
import Control.Monad.Error


import Language.Lua.Parser
import Language.Lua.Token
import Language.Lua.AST
import Language.Lua.VM.Value
import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Core
import Language.Lua.GenCode
import Language.Lua.GenCode.State
import Language.Lua.GenCode.Assemble

-- import Control.Applicative      	( (<$>) )

import System.IO

-- ------------------------------------------------------------

type Process = ErrorT String IO

runProcess :: (arg -> Process res) -> arg -> IO (Either String res)
runProcess p inp = runErrorT (p inp)

scan :: String -> Process TokenStream
scan inp
    = either (abort . ("lexical error in program: " ++) . show) return $ tokenize "" inp

parse :: TokenStream -> Process Block
parse ts
    = either (abort . ("syntax error in program: " ++) . show) return $ parse_chunk "" ts

gencode :: Block -> Process Code
gencode ast
    | null . theCErrs $ errs
        = return code
    | otherwise
        = abort . ("semantic error(s) in program: " ++) . show $ errs
    where
      (code, errs) = compileProg ast

assemble :: Code -> Process LuaModule
assemble
    = return . assembleProg

exec :: LuaModule -> Process LuaState
exec prog
    = do (_res, state) <- liftIO $ runLua execLoop prog emptyLuaState
         return state

abort :: String -> Process res
abort msg
    = do liftIO $ hPutStrLn stderr msg
         throwError msg

showRes :: Show res => res -> Process ()
showRes res
    = liftIO (print res) >> return ()

dumpState :: LuaState -> Process ()
dumpState state
    = liftIO $ dumpLuaState state >>= (putStrLn . ("\n" ++))

-- ------------------------------------------------------------

sc :: String -> IO (Either String ())
sc = runProcess $ scan >=> showRes

pa :: String -> IO (Either String ())
pa = runProcess $ scan >=> parse >=> showRes

gc :: String -> IO (Either String ())
gc = runProcess $ scan >=> parse >=> gencode >=> showRes

as :: String -> IO (Either String ())
as = runProcess $ scan >=> parse >=> gencode >=> assemble >=> showRes

ex :: String -> IO (Either String ())
ex = runProcess $ scan >=> parse >=> gencode >=> assemble >=> exec >=> dumpState

-- ------------------------------------------------------------

p0 = "do vm.traceOn(); x = 0; while x < 5 do x = x + 1 end end"
p1 = "do x = 0; while x < 5 do x = x + 1 end; print(\"x=\"..x) end"
p2 = "print(type(1))"
p3 = "print(assert(true,1))"
p4 = "print(assert(1==2,1))"
p5 = "do vm.traceOn(); x,y,z = g(0,1),2,f(3,4,5) end"
p6 = "do vm.traceOn(); return f(1,2),f(3,4,5) end"
p7 = "do vm.traceOn(); x = y and z end"
p8 = "do vm.traceOn(); x = y or z end"
p9 = "do print(1,2,3) end"
p10 = "do t = {1,2,3,[2]=55, [1]=11,4,5}; for i=1,5,1 do print(\"i=\",i,\"t[i]=\",t[i]) end end"
p11 = "do for k,v in pairs({1,2,3,[2]=55, [1]=11,4,5}) do print(k,v) end end"
p12 = "do f = function (x) local y = x; return y; end; return f; end"
p13 = unlines [ "do local z = 1;"
              , "   local f = function (x)"
              , "               local y = z;"
              , "               return y;"
              , "             end;"
              , "   return f;"
              , "end"
              ]
p14 = unlines
      [ "do"
      , "  x1, x2 = 23, 42;"
      , "  local x1 = x1;"
      , "  function f1() return 42; end;"
      , "  function f2(x,y) return x-y, x+y, f1(); end;"
      , "  function f3(x,...) return x,args; end"
      , "  f1(27);"
      , "  x1, x2 = f2(x1,x2);"
      , "  x1, x2 = f3(1,2,3);"
      , "end"
      ]

cc :: String -> Either String (Code, CErrs)
cc inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b
                     -> Right $ compileProg b

aa :: String -> Either String (MCode, CErrs)
aa inp
    = either Left (Right . first assembleProg) $ cc inp

pp :: String -> Either String Block
pp inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b -> Right b

-- ------------------------------------------------------------

