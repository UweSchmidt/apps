module Language.Tcl.QuoteListValue
    ( stringToList
    , escapeArg
    , inBraces
    , isBraceArg
    , isCharArg
    )
where

import Language.Tcl.Parser	( isBraceArg
                                , isCharArg
                                )

-- ------------------------------------------------------------

stringToList :: String -> String
stringToList s
    | null s       = inBraces  s
    | isCharArg  s =           s	-- try to avoid escapes and braces
    | isBraceArg s = inBraces  s	-- try to avoid escapes
    | otherwise    = escapeArg s	-- some chars must be escaped

escapeArg	:: String -> String
escapeArg
    = concatMap esc'
      where
        esc' c
            | c `elem` " \t\n\r\\{}\""
                = '\\' : c : ""
            | otherwise
                =        c : ""

inBraces :: String -> String
inBraces
    = ("{" ++) . (++ "}")

-- ------------------------------------------------------------
