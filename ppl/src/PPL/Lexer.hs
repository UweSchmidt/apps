module PPL.Lexer where

import PPL.Symbol

import Data.Char
import Data.List

lexer           :: String -> [Symbol]

lexer []
    = [(Eof, "<end of file>")]

lexer cs@(c:cs1)
    | isSpace c = lexer cs1     -- separators
    | isAlpha c = lexId cs      -- ids or keywords
    | isDigit c = lexNum cs     -- numbers

lexer ('\"':cs)
    = lexString "" cs

lexer ('-':'-':cs)              -- comments like this one
    = lexComment cs

lexer cs                        -- symbols
    | found     = (tok, cs1)
                  : lexer (drop (length cs1) cs)
    where
    sys         = filter (\ (ts,_) ->
                          isPrefixOf ts cs
                         ) symbols
    found       = not (null sys)
    sy          = head sys
    (cs1, tok)  = sy
    symbols = [ (":=",  Assign)
              , (":",   Colon)
              , (",",   Comma)
              , ("(",   LPar)
              , (")",   RPar)
              , ("[",   LBr)
              , ("]",   RBr)
              , (";",   Semicolon)
              , (".",   Dot)
              , ("+",   PlusOp)
              , ("-",   MinusOp)
              , ("*",   MultOp)
              , ("=>",  ImplOp)
              , ("=",   EqOp)
              , ("/=",  NeOp)
              , ("/",   DivOp)
              , (">=",  GeOp)
              , (">",   GrOp)
              , ("<=>", EquivOp)
              , ("<=",  LeOp)
              , ("<",   LtOp)
              ]

lexer (c:_cs)
    = error ("illegal character: " ++ [c])

-- --------------------

lexNum          :: String -> [(Token, String)]

lexNum cs
    = lexNum1 ns cs1
      where
      (ns, cs1) = span isDigit cs

-- --------------------

lexNum1         :: String -> String -> [(Token, String)]

lexNum1 ns ('.':cs1)
    | hasFraction
        = lexNum2 (ns ++ "." ++ fraction) cs2
    where
    (fraction, cs2)     = span isDigit cs1
    hasFraction         = not (null fraction)

lexNum1 ns cs
    = (IntConst, ns) : lexer cs

-- --------------------

lexNum2         :: String -> String -> [(Token, String)]

lexNum2 ns (c1:c2:cs2@(c3:_))
    | c1 `elem` "Ee"
      &&
      c2 `elem` "+-"
      &&
      isDigit c3
          = (FloatConst, ns ++ [c1] ++ [c2] ++ exp2)
            : lexer rest2
            where
            (exp2, rest2)       = span isDigit cs2

lexNum2 ns (c1:cs1@(c2:_))
    | c1 `elem` "Ee"
      &&
      isDigit c2
          = (FloatConst, ns ++ [c1] ++ "+" ++ exp1)
            : lexer rest1
            where
            (exp1, rest1)       = span isDigit cs1

lexNum2 ns cs
    = (FloatConst, ns) : lexer cs

-- --------------------

lexId           :: String -> [(Token, String)]

lexId cs
    = (token, ident) : lexer cs1
    where
    (ident, cs1)        = span isAlphaNum cs
    token               = keyword (lookup ident keywords)
    keyword (Just kw)   = kw
    keyword Nothing     = IdentSy
    keywords            = [ ("and",     AndSy)
                          , ("begin",   BeginSy)
                          , ("boolean", BoolSy)
                          , ("div",     DivSy)
                          , ("do",      DoSy)
                          , ("else",    ElseSy)
                          , ("elseif",  ElseIfSy)
                          , ("end",     EndSy)
                          , ("endif",   EndIfSy)
                          , ("endwhile",EndWhileSy)
                          , ("return",  ReturnSy)
                          , ("false",   BoolConst)
                          , ("float",   FloatSy)
                          , ("if",      IfSy)
                          , ("int",     IntSy)
                          , ("list",    ListSy)
                          , ("max",     MaxSy)
                          , ("min",     MinSy)
                          , ("mod",     ModSy)
                          , ("not",     NotSy)
                          , ("of",      OfSy)
                          , ("or",      OrSy)
                          , ("picture", PicSy)
                          , ("repeat",  RepeatSy)
                          , ("string",  StringSy)
                          , ("then",    ThenSy)
                          , ("true",    BoolConst)
                          , ("until",   UntilSy)
                          , ("var",     VarSy)
                          , ("function",FunctionSy)
                          , ("procedure", ProcedureSy)
                          , ("while",   WhileSy)
                          , ("xor",     XorSy)
                          ]

-- --------------------

lexComment              :: String -> [Symbol]                      

lexComment ('\n':cs)
    = lexer cs          -- end of line
                        -- => end of comment
lexComment (_:cs)
    = lexComment cs

lexComment []
    = lexer []          -- eof in comment

-- --------------------

lexString       :: String -> String -> [(Token, String)]
                                -- end of string const
lexString ss ('\"':cs)
    = (StringConst, reverse ss) : lexer cs

lexString ss ('\\':c:cs)        -- quoted char
    = lexString ((decodeQuoteChar c):ss) cs

lexString ss (c:cs)             -- unquoted char
    = lexString (c:ss) cs

                        -- end of file in string const
lexString _ []
    = error "end of input in string constant"

-- --------------------

decodeQuoteChar         :: Char -> Char

decodeQuoteChar 'n' = '\n'
decodeQuoteChar 't' = '\t'
decodeQuoteChar 'r' = '\r'
decodeQuoteChar 'b' = '\b'
decodeQuoteChar ch  = ch

showLex                 :: [Symbol] -> String
showLex
    = concat . map showToc
      where
      showToc (tok, txt)
          = take 16 (show tok ++ replicate 16 ' ') ++
            show txt ++ "\n"
