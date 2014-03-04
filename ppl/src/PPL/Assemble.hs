module PPL.Assemble where

import PPL.Instructions

type LabTab     = [(Label, Int)]

assemble        :: Executable -> Executable
assemble (is, ds)
    = (assemble1 is, ds)

assemble1       :: Code -> Code
assemble1 is
    = let
      labTab    = buildLabTab [] 0 is
      is'       = filter noLabelInstr is
      in
      zipWith (compDispl labTab) [0..] is'

--

buildLabTab     :: LabTab -> Int -> Code -> LabTab

buildLabTab lt _ []
    = lt

buildLabTab lt i ((Label l) : is)
    = buildLabTab ((l,i) : lt) i is

buildLabTab lt i (_ : is)
    = buildLabTab lt (i+1) is

-- lookup label table

labVal          :: Label -> LabTab -> Int
labVal lab lt
    = maybe 0 id (lookup lab lt)


noLabelInstr            :: Instr -> Bool

noLabelInstr (Label _)  = False
noLabelInstr _          = True

compDispl       :: LabTab -> Int -> Instr -> Instr

compDispl lt i (Branch cond (Symb l))
    = Branch cond (Disp (labVal l lt - i))

compDispl lt i (Jump (Symb l))
    = Jump (Disp (labVal l lt - i))

compDispl lt i (PushJ (Symb l))
    = PushJ (Disp (labVal l lt -i))

compDispl _ _ instr
    = instr
