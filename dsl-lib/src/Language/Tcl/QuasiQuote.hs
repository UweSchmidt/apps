{-# LANGUAGE TemplateHaskell #-}

-- ------------------------------------------------------------

-- | QuasiQuoter for Tcl source code
--
-- The "tcl" quoter contains a multi-line string with some interpolation:
-- the leading newline is trimmed, carriage returns stripped,
-- double quotes and backslashes are escaped.
--
-- the source was stolen from Audrey Tang's string-qq package and adapted for Tcl source code

module Language.Tcl.QuasiQuote
    ( tcl )
where

import Language.Haskell.TH.Quote

tcl :: QuasiQuoter
tcl = QuasiQuoter
      ((\a -> [| a|]) . trimLeadingNewline . escapeChars . removeCRs)
      (error "Cannot use q as a pattern")
      (error "Cannot use q as a type")
      (error "Cannot use q as a dec")
    where
      removeCRs
          = filter (/= '\r')
      trimLeadingNewline ('\n':xs)
          = xs
      trimLeadingNewline xs
          = xs
      escapeChars
          = concatMap esc
          where
            esc c
                | c `elem` ['\\', '\"'] = '\\' : c : []
                | otherwise             =        c : []

-- ------------------------------------------------------------
