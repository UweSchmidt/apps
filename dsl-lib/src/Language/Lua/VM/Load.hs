module Language.Lua.VM.Load
where

-- import Control.Monad.Error
-- import Control.Monad.Reader
import Control.Monad.State

-- import Language.Lua.VM.Instr
import Language.Lua.VM.Types
import Language.Lua.VM.Value
import Language.Lua.VM.Core
import Language.Lua.VM.CoreFct

loadFcts :: [(String, NativeAction)]
loadFcts
    = [ ( "loadstring"
        , oneOrMoreArgs "loadstring"
          >=> checkArgs "loadString" [check1 checkString, anyArg]
          >=> loadString
        )
      ]

loadString :: NativeAction
loadString [v]
    = loadString [v, v]
loadString (vs : vmsg : _)
    = undefined
