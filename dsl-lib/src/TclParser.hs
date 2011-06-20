module TclTest
where

import Language.Tcl.AbstractSyntax
import Language.Tcl.Parser             ( parseTclProg
                                       , parseTclList
                                       , TclParser
                                       )
import Language.Tcl.Show

-- ------------------------------------------------------------

p ps s = parse (eofP ps) "" s
