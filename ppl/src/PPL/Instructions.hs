module PPL.Instructions where

type Code       = [Instr]

data Instr
    = LoadI             Int             -- load int const
    | LoadF             Double          -- load float const
    | LoadS             String          -- load string const
    | LoadU                             -- load undefined value
    | LoadEL                            -- load empty list
    | Load              Address         -- read data memory
    | Store             Address         -- write data memory
    | Pop                               -- remove arg from stack
    | Dup                               -- duplicate top of stack
    | Compute           Opcode          -- evaluate something
    | SysCall           Subroutine      -- system call (svc)
    | PushJ             Dest            -- subroutine call
    | PopJ                              -- computed jump (subroutine return)
    | Entry             Int             -- allocate stack frame
    | Exit                              -- delete stack frame
    | Branch            Bool Dest       -- gotos and branches
    | Jump              Dest
    | Label             Label           -- symbolic jump target
    | IllegalInstr      String          -- not yet implemented
    deriving (Eq, Show)

data Address
    = LocA Int          -- local address
    | AbsA Int          -- global address
    deriving (Eq, Show)

data Dest
    = Symb      Label
    | Disp      Int
    deriving (Eq, Show)

data Opcode
    = OPabort
    | OPabove
    | OPaddf
    | OPaddi
    | OPappendl
    | OPbitmap
    | OPblack
    | OPblackAndWhite
    | OPconcatHorizontal
    | OPconcatVertical
    | OPconcl
    | OPconcs
    | OPcut
    | OPdecri
    | OPdiff
    | OPdivf
    | OPdivi
    | OPeqf
    | OPeqi
    | OPf2s
    | OPflipDiagonal
    | OPflipHorizontal
    | OPflipVertical
    | OPgamma
    | OPgef
    | OPgei
    | OPgrey
    | OPgtf
    | OPgti
    | OPheight
    | OPi2f
    | OPi2s
    | OPincri
    | OPindexl
    | OPinverseDiff
    | OPinverseMean
    | OPinvert
    | OPisemptyl
    | OPlengthl
    | OPmaxf
    | OPmaxi
    | OPmaxp
    | OPmean
    | OPmergeHorizontal
    | OPmergeVertical
    | OPminf
    | OPmini
    | OPminp
    | OPmodi
    | OPmulf
    | OPmuli
    | OPmulp
    | OPpartitionHorizontal
    | OPpartitionVertical
    | OPpaste
    | OPconsl
    | OPreduceColor
    | OPreplicate
    | OPresize
    | OProtate
    | OPround
    | OPscale
    | OPshift
    | OPshrink
    | OPsideBySide
    | OPsplitHorizontal
    | OPsplitVertical
    | OPsubf
    | OPsubi
    | OPtaill
    | OPterminate
    | OPtrunc
    | OPwhite
    | OPwidth
    deriving (Eq, Show)

type Arg        = String
type Comment    = String
type Label      = String
type Subroutine = String

type DataSeg    = Int

type Executable = (Code, DataSeg)
