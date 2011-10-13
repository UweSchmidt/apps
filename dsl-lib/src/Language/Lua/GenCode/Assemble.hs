module Language.Lua.GenCode.Assemble
where

import Prelude hiding         ( lookup )

import Data.Map               ( Map
                              , empty
                              , lookup
                              , insert
                              )
import Data.Maybe             ( fromMaybe )

import Language.Lua.VM.Types

-- ------------------------------------------------------------

type LMap = Map Label Int

-- ------------------------------------------------------------
--
-- assembling in this simple case is just
-- eliminating labels from the code and substituting
-- labels by distances in the instructions referencing
-- code points
--
-- the label substitution is done with dynamic programming
-- in a single pass. The result is a new code sequence and the
-- label map. This map is already looked up when scanning
-- the code sequence.
-- This requires lazy evaluation

assembleProg :: Code -> MCode
assembleProg (Code acode)
    = MCode mcode
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
                      disp l = D $ (fromMaybe (error "assembleProg: undefined label")
                                    .
                                    lookup l $ tab
                                   ) - ic
                      ins' =
                          case ins of
                            Jump     (M l) -> Jump     $ disp l
                            Branch c (M l) -> Branch c $ disp l
                            Closure  (M l) -> Closure  $ disp l
                            _              -> ins
                  in (lt', ins' : code')

-- ------------------------------------------------------------