module PPL.OptimizeInstr
    ( optimizeInstr
    )
where

import PPL.Instructions

import Data.Maybe

type LabSubst   = [(Label, Label)]

optimizeInstr   :: Executable -> Executable
optimizeInstr (is, ds)
    = ((peephole . removeUnusedLabels . jumpChaining . optimizeTailRecursion) is, ds)

jumpChaining    :: Code -> Code
jumpChaining is
    = let
      labTab    = buildLabSubst [] is
      is'       = renameLabels labTab is
      in
      is'

buildLabSubst   :: LabSubst -> Code -> LabSubst

-- real jump chaining
buildLabSubst lt (  (Label l1)
                  : (Jump (Symb l2))
                  : is2)
    = buildLabSubst lt1 is2
      where
      lt1 = insLabSubst l1 l2 lt

-- labels with equal values are collapsed
buildLabSubst lt (        (Label l1)
                   : is1@((Label l2)
                          : _))
    = buildLabSubst lt1 is1
      where
      lt1 = insLabSubst l1 l2 lt

-- default rules
buildLabSubst lt (_:is1)
    = buildLabSubst lt is1

buildLabSubst lt []
    = lt


-- --------------------

insLabSubst     :: Label -> Label -> LabSubst -> LabSubst
insLabSubst l1 l2 lt
    = if (l2,l1) `elem` lt || l1 == l2
      then lt
      else (l1, l2) : lt'
           where
           lt'  = map renameWith lt
           renameWith (l1',l2')
               | l2' == l1
                   = (l1', l2)
           renameWith p
               = p

newLabName      :: LabSubst -> Label -> Label
newLabName lt l
    = case lookup l lt of
      Just l1 -> l1
      Nothing -> l

renameLabels    :: LabSubst -> Code -> Code

-- remove alias labels

renameLabels lt ((Label l1) : is1)
    | isJust (lookup l1 lt)
        =  renameLabels lt is1

-- default
renameLabels lt (i1:is)
    = renameLab lt i1 : renameLabels lt is

renameLabels _ []
    = []

renameLab       :: LabSubst -> Instr -> Instr

renameLab lt (Branch cond (Symb l1))
    = Branch cond (Symb (newLabName lt l1))

renameLab lt (Jump (Symb l1))
    = Jump (Symb (newLabName lt l1))

renameLab _lt i
    = i

-- --------------------

usedLabels      :: Code -> [Label]
usedLabels
    = concat . map lab
      where
      lab (Branch _ (Symb l))   = [l]
      lab (Jump     (Symb l))   = [l]
      lab _                     = []

removeUnusedLabels      :: Code -> Code
removeUnusedLabels cs
    = filter usedLabel cs
      where
      used = usedLabels cs

      usedLabel (Label l@(c:_))
          = c == '_'                    -- global label
            ||
            l `elem` used               -- label is referenced in jump or branch
      usedLabel _
          = True

-- --------------------

-- peephole optimizations
-- local instruction optimization

peephole                :: Code -> Code

-- remove instructions following unconditional jumps

peephole (i1@(Jump _l1)
          : i2
          : is
         )
    | noLabel i2
      = peephole (i1:is)

-- remove jump to following instruction

peephole (Jump (Symb l1)
          : is2@(Label l2
                 : _
                )
         )
    | l1 == l2
        = peephole is2

-- remove conditional branch to following instruction

peephole (Branch _ (Symb l1)
          : is2@(Label l2
                 : _
                )
         )
    | l1 == l2
        = Pop
          : peephole is2

-- conditional branches with constants

peephole (LoadI val
          : Branch cond lab
          : is
         )
    | (val /= 0) == cond
        = peephole (Jump lab : is)
    | otherwise
        = peephole is

-- optimize store load sequences

peephole (Store a1
          : Load a2
          : is
         )
    | a1 == a2
        = Dup
          : peephole (Store a1 : is)

-- optimize increment and decrement

peephole (LoadI 1
          : Compute OPaddi
          : is
         )
    = Compute OPincri
      : peephole is

peephole (LoadI 1
          : Compute OPsubi
          : is
         )
    = Compute OPdecri
      : peephole is

peephole (Pop
          : LoadU
          : is
         )
    = peephole is

-- next step

peephole (i1:is1)
    = i1 : peephole is1

peephole []
    = []

-- --------------------

noLabel                 :: Instr -> Bool

noLabel (Label _)       = False
noLabel _               = True

-- --------------------

optimizeCalls           :: LabSubst -> Code -> Code
optimizeCalls lt cs@( Label sl
                      : Entry _
                      : Store _
                      : Label sl1
                      : cs4
                    )
    = take 4 cs
      ++
      optimize1Fct cs4
      where
      el1 = 'e' : sl
      el = newLabName lt el1

      optimize1Fct cs'@( Exit           -- function exit detected
                         : PopJ         -- optimize rest
                         : rest'
                       )
          = take 2 cs'
            ++
            optimizeCalls lt rest'

      optimize1Fct ( PushJ fct@(Symb target)
                     : Jump (Symb l1)
                     : rest
                   )
                                        -- pure tail recursion detected
                                        -- subroutine call substituted
                                        -- by a jump to routine start
          | target == sl && newLabName lt l1 == el
              = Jump (Symb sl1)
                : optimize1Fct rest
                                        -- tail recursion to another function
                                        -- load return address
                                        -- remove stack frame
                                        -- and jump
          | newLabName lt l1 == el
              = Load (LocA 0)
                : Exit
                : Jump fct
                : optimize1Fct rest

                                        -- same as above for calls
                                        -- directly in front of exit code
      optimize1Fct ( PushJ fct@(Symb target)
                     : cs1@( Label l1
                             : _rest
                           )
                   )
          | target == sl && newLabName lt l1 == el
              = Jump (Symb sl1)
                : optimize1Fct cs1
                
          | newLabName lt l1 == el
              = Load (LocA 0)
                : Exit
                : Jump fct
                : optimize1Fct cs1

                                        -- continue search for calls
      optimize1Fct (c : cs1)
          = c : optimize1Fct cs1

      optimize1Fct []
          = []

optimizeCalls lt (c1 : cs1)
    = c1 : optimizeCalls lt cs1

optimizeCalls _ []
    = []

-- optimize tail recursion
-- 1. step: collect all function entry end exit points
-- 2. step: look for function calls

optimizeTailRecursion   :: Code -> Code
optimizeTailRecursion cs
    = optimizeCalls labTab cs
      where
      labTab = buildLabSubst [] cs

-- --------------------
