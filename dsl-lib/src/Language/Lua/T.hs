module T
where

import Control.Arrow ( first )

import Language.Lua.Parser
import Language.Lua.Token
import Language.Lua.AST
import Language.Lua.VM.Instr
import Language.Lua.GenCode
import Language.Lua.GenCode.State
import Language.Lua.GenCode.Assemble

-- ------------------------------------------------------------

cc :: String -> Either String (Code, CErrs)
cc inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b
                     -> Right $ compileProg b

aa :: String -> Either String (Code, CErrs)
aa inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b
                     -> Right $ (first assembleProg) $ compileProg b

pp :: String -> Either String Block
pp inp
    = case tokenize "" inp of
        Left err -> Left $ show err
        Right ts
            -> case parse_chunk "" ts of
                 Left err -> Left $ show err
                 Right b -> Right b

-- ------------------------------------------------------------
