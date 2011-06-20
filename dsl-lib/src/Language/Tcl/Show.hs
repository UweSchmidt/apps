module Language.Tcl.Show
    ( showTclProg
    , showTclCmd
    , showTclList
    , showTclArgs
    , showTclArg
    , showTclSubst
    )
where

import Data.List	( intercalate )

import Language.Tcl.AbstractSyntax
import Language.Tcl.Parser

import Text.Parsec	( parse
                        , noneOf
                        , many
                        )

-- ------------------------------------------------------------

showTclProg :: TclProg -> String
showTclProg
    = intercalate "\n" . map showTclCmd . _tclProg

showTclCmd :: TclCmd -> String
showTclCmd
    = showTclArgs . _tclCmd

showTclList :: TclList -> String
showTclList
    = showTclArgs . _tclList

showTclArgs :: [TclArg] -> String
showTclArgs
    = intercalate " " . map showTclArg 

showTclArg :: TclArg -> String
showTclArg (TclArg a)
    | null s       = inBraces  s
    | isCharArg  s =           s	-- try to avoid escapes and braces
    | isBraceArg s = inBraces  s	-- try to avoid escapes
    | otherwise    = escapeArg s	-- some chars must be escaped
    where
      s = concatMap showTclSubst a

showTclSubst :: TclSubst -> String
showTclSubst (TLit s)
    = s
showTclSubst (TVar n)
    = "$" ++ n
showTclSubst (TEval pg)
    = "[" ++ showTclProg pg ++ "]"

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
