module T
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

p0 = "do x = 0; while x < 5 do x = x + 1 end end"
p1 = "do x = 0; while x < 5 do x = x + 1 end; print(\"x=\",x) end"

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

