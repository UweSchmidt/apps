module T
where

import Language.Lua.Parser
import Language.Lua.Token
import Language.Lua.AST
import Language.Lua.Instr
import Language.Lua.Compile
import Language.Lua.CompileState

-- ------------------------------------------------------------

cc :: String -> Either String (ACode, CErrs)
cc inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b
                     -> Right $ compileProg b

pp :: String -> Either String Block
pp inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b -> Right b

-- ------------------------------------------------------------
