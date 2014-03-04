module PPL.Loader
    (loadExecutable
    )
    where

import PPL.Instructions
import PPL.MachineArchitecture

import Data.Array

-- -------------------------------------------------------------------

-- load executable and create initial machine state

loadExecutable  :: Executable -> MS
loadExecutable (is, ds)
    = MS instr' pc' mem' stack' frames' status'
      where
      instr'    = listArray (0, length is - 1) is
      pc'       = 0
      mem'      = (replicate ds VUndef)
      stack'    = []
      frames'   = []
      status'   = Ok

-- -------------------------------------------------------------------
