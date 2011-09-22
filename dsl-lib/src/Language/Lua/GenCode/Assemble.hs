module Language.Lua.GenCode.Assemble
where

import Prelude hiding         ( lookup )

import Data.Map               ( Map
                              , empty
                              , lookup
                              , insert
                              )
import Data.Maybe             ( fromJust )

import Language.Lua.VM.Instr

type LMap = Map Label Int

-- ------------------------------------------------------------

assembleProg :: Code -> Code
assembleProg (Code acode)
    = Code mcode
    where
      (tab, mcode) = scanCode empty 0 acode
      scanCode lt _ic []
          = (lt, [])
      scanCode lt ic (instr : code)
          = case instr of
              Label l ->
                  let (lt', code') = scanCode lt ic code
                  in (insert l ic lt', code')
              ins ->
                  let (lt', code') = scanCode lt (ic + 1) code
                      disp l = D $ (fromJust . lookup l $ tab) - ic
                      ins' =
                          case ins of
                            Jump     (L l) -> Jump     $ disp l
                            Branch c (L l) -> Branch c $ disp l
                            Closure  (L l) -> Closure  $ disp l
                            _              -> ins
                  in (lt', ins' : code')

-- ------------------------------------------------------------