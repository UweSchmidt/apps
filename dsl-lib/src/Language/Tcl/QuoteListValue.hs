module Language.Tcl.QuoteListValue
where

import Language.Tcl.Parser

import Text.Parsec	( parse
                        , noneOf
                        , many
                        )

-- ------------------------------------------------------------

stringToList :: String -> String
stringToList s
    | null s       = inBraces  s
    | isCharArg  s =           s	-- try to avoid escapes and braces
    | isBraceArg s = inBraces  s	-- try to avoid escapes
    | otherwise    = escapeArg s	-- some chars must be escaped

isCharArg	:: String -> Bool
isCharArg s
    = case parse (eofP lchars) "" s of
        Left _ -> False
        Right _ -> True
      where
        lchars :: TclParser String
        lchars
            = do c1 <-        noneOf $ br ++ esc ++ ws ++ nl
                 cs <- many $ noneOf $       esc ++ ws ++ nl
                 return $ c1 : cs


isBraceArg	:: String -> Bool
isBraceArg s
    = case parse (eofP braceContent) "" s of
        Left _ -> False
        Right _ -> True


escapeArg	:: String -> String
escapeArg
    = concatMap esc'
      where
        esc' c
            | c `elem` (ws ++ nl ++ esc ++ br ++ dq ++ "}")
                = '\\' : c : ""
            | otherwise
                =        c : ""

inBraces :: String -> String
inBraces
    = ("{" ++) . (++ "}")

-- ------------------------------------------------------------
