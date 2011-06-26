{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.Tcl.Parser
    ( parseTclProg
    , parseTclList
    , parseTclArgs
    , isCharArg
    , isBraceArg
    )
where

import Control.Applicative 	( (<$>) )
import Control.Monad 		( mzero )

import Language.Tcl.AbstractSyntax

import Text.Parsec

-- ------------------------------------------------------------

type TclParser = Parsec String ()

-- ------------------------------------------------------------
--
-- the main Tcl parsers

-- parse a string as a sequence of commands
parseTclProg :: String -> Either ParseError TclProg
parseTclProg
--    = parse (eofP tprog) ""
    = parse aTclProg ""

-- parse a string as a sequence of args, newline and ; are normal chars
parseTclArgs :: String -> Either ParseError TclCmd
parseTclArgs
    = parse aTclCmd ""
{-
    = parse ( eofP $
              cargs >>= return . TclCmd
            ) ""
-}

-- parse a string as a sequence of list elements, newline, ;, $ and [ are normal chars
parseTclList :: String -> Either ParseError TclList
parseTclList
    = parse aTclList ""
{-
    = parse ( eofP $
              largs >>= return . TclList
            ) ""
-}

isCharArg	:: String -> Bool
isCharArg s
    = either (const False) (const True) $
      parse (eofP lchars) "" s
    where
      lchars :: TclParser String
      lchars
          = do c1 <-        noneOf $ "{\\ \t\n\r"
               cs <- many $ noneOf $  "\\ \t\n\r"
               return $ c1 : cs

isBraceArg	:: String -> Bool
isBraceArg s
    = either (const False) (const True) $
      parse (eofP $ bracesContent $ inBraceSubst defaultNoSubst) "" s

-- ------------------------------------------------------------
{-
tprog :: TclParser TclProg
tprog
    = tprog' ""

tprog' :: String -> TclParser TclProg
tprog' na
    = do
      cs <- tcmds na
      return $ TclProg cs

tcmds :: String -> TclParser [TclCmd]
tcmds na
    = do
      c  <- tcmd na
      cs <- option [] $ do nl1
			   tcmds na
      return $ c ++ cs

tcmd :: String -> TclParser [TclCmd]
tcmd na
    = do
      ws0
      option [] $ do
		  c  <- targ  na
		  al <- targs na
		  return $ [TclCmd (c : al)]

targs :: String -> TclParser [TclArg]
targs
    = targs' ws1 (tvar <|> tbracket) vc

cargs :: TclParser [TclArg]
cargs
    = targs' wsnl1 (tvar <|> tbracket <|> tsemi) vc ""

largs :: TclParser [TclArg]
largs
    = targs' wsnl1 parserZero "" ""

targs' :: TclParser () ->		-- ^ whitespace parser, newline is included for list parser
          TclParser TclSubst ->         -- ^ variable and command substituion parser, mzero for list parser
          String ->                     -- ^ variable and command chars $ and [, or "" for list parser
          String ->                     -- ^ command follow chars
          TclParser [TclArg]
targs' tws tsubst vc' na
    = do option () tws
         ( targs1 <|> return [] )
    where
    targs1
	= do
	  a  <- targ'      tsubst vc' na
	  as <- targs' tws tsubst vc' na
	  return $ a : as
-}
-- ------------------------------------------------------------
--
-- 3 variants of args
-- .1 unquoted sequence of chars
-- .2 double quoted sequence of chars
-- .3 text within braces
-- .4 commands within brackets

ws, nl, nls, vc, esc, br, dq :: String
ws  = " \t"
nl  = "\n\r"
nls  = nl ++ ";"
vc  = "$["	-- variable, command, escape
esc = "\\"
br  = "{"
dq  = "\""

{-
targ :: String -> TclParser TclArg
targ
    = targ' (tvar <|> tbracket) vc

carg :: String -> TclParser TclArg
carg
    = targ' (tvar <|> tbracket <|> tsemi) vc

larg :: String -> TclParser TclArg
larg
    = targ' parserZero ""

targ' :: TclParser TclSubst -> String -> String -> TclParser TclArg
targ' tsubst vc' na
    = do
      xs <- tcharsArg tsubst vc' na <|> tdquoteArg tsubst vc' na <|> tbraceArg na
      return $ TclArg xs

tcharsArg :: TclParser TclSubst -> String -> String -> TclParser [TclSubst]
tcharsArg tsubst vc' na
    = do
      s1 <- (tchar1 <|> tesc <|> tsubst)
      xs <- many (tchar (na ++ vc' ++ esc ++ ws ++ nls) <|> tesc <|> tsubst)
      return $ s1 : xs
    where
    tchar1
	= do
	  c <-        noneOf $ na ++ br ++ vc' ++ esc ++ ws ++ nls
	  s <- many $ noneOf $ na ++       vc' ++ esc ++ ws ++ nls
	  return $ TLit (c : s)

tdquoteArg :: TclParser TclSubst -> String -> String -> TclParser [TclSubst]
tdquoteArg tsubst vc' na
    = do
      s <- between (char '\"') (char '\"') $
	   many $
	   tchar (dq ++ vc' ++ esc) <|> tesc <|> tsubst
      noExtraChar na
      return s

tbraceArg  :: String -> TclParser [TclSubst]
tbraceArg na
    = do
      a <- tbrace
      noExtraChar na
      return [a]

-- ------------------------------------------------------------

tchar :: String -> TclParser TclSubst
tchar notAllowed
    = do
      s <- many1 $ noneOf notAllowed
      return $ TLit s

tesc :: TclParser TclSubst
tesc
    = do
      char '\\'
      ( do
	c <- escChar
        return $ TLit [c] )
        <|>					-- 1 to 3 digit octal number
        ( do od <- mToN octDigit 1 3
             return $ TLit $ toEnum (((read $ "0o" ++ od) :: Int) `mod` 256) : ""
        )
        <|>					-- 2 digit hex number
        try ( do char 'x'
                 hd <- many1 hexDigit
                 let hd' = reverse . take 2 . reverse $ hd
                 return $ TLit $ toEnum ((read $ "0x" ++ hd') :: Int) : ""
            )
        <|>					-- 2 digit hex number
        try ( do char 'u'
                 hd <- mToN hexDigit 1 4
                 return $ TLit $ toEnum ((read $ "0x" ++ hd) :: Int) : ""
            )
	<|>
	( do
	  c <- anyChar
	  return $ TLit [c]
	)
    where
    escChar	-- octal, hex and unicode escapes are not handled
	=  ( do
	     c <- oneOf "abfnrtv"
	     return $ read $ "\'\\" ++ [c] ++ "\'"
	   )
      
tvar :: TclParser TclSubst
tvar
    = do
      char '$'
      option (TLit "$") $ do
			  v <- varName
			  return $ TVar v
    where
    varName		-- not implemented: array vars
	= ( do
	    xs <- many1 (many1 (letter <|> digit <|> char '_') <|> namespaceSep)
	    return $ concat xs
	  )
	  <|>
	  ( do
	    s <- between (char '{') (char '}') $ many1 $ noneOf "}"
	    return s
	  )
	where
	namespaceSep
	    = do
	      c1 <- char ':'
	      cs <- many1 $ char ':'
	      return $ c1 : cs

tbracket :: TclParser TclSubst
tbracket
    = do
      s <- between (char '[') (char ']') $ tprog' "]"
      return $ TEval s
      

tbrace :: TclParser TclSubst
tbrace
    = between (char '{') (char '}') $
      do
      s <- braceContent
      return $ TLit s

tsemi :: TclParser TclSubst
tsemi = char ';' >> (return . TLit $ ";")

-- ------------------------------------------------------------

braceContent :: TclParser String
braceContent
    = do xs <- many (bchars <|> bescchar <|> bbrace)
	 return $ concat xs
    where
    bchars
	= many1 (noneOf "{}\\")

    bbrace :: TclParser String
    bbrace
	= do
	  s <- between (char '{') (char '}') braceContent
	  return $ "{" ++ s ++ "}"

-- ------------------------------------------------------------

bescNL :: TclParser Char
bescNL
    = try (string "\\\n")
      >> many (oneOf " \t\r")
      >> return ' '

bescCH :: TclParser String
bescCH
    = do
      char '\\'
      c <- anyChar
      return $ "\\" ++ [c]

bescchar :: TclParser String
bescchar
    = ( bescNL >> return " " )
      <|>
      bescCH

noExtraChar :: String -> TclParser ()
noExtraChar na
    = notFollowedBy $ noneOf $ na ++ ws ++ nls

ws1 ::  TclParser ()
ws1 = ws1' >> return ()

ws0 ::  TclParser ()
ws0 = option () ws1

wsnl1 ::  TclParser ()
wsnl1 = wsnl1' >> return ()

nl1 :: TclParser ()
nl1 = nl1' >> return ()

nl0 ::  TclParser ()
nl0 = option () nl1
-}
-- ------------------------------------------------------------

toN :: TclParser r -> Int -> TclParser [r]
toN p n
    | n == 0
        = return []
    | n > 0
        = option [] $
          do r1 <- p
             rs <- toN p $ n - 1
             return $ r1 : rs
    | otherwise -- n < 0
        = parserZero
          
mToN :: TclParser r -> Int -> Int -> TclParser [r]
mToN p n m
    | n == 0
        = toN p m
    | otherwise
        = do r1 <- p
             rs <- mToN p (n-1) (m-1)
             return $ r1 : rs

-- ------------------------------------------------------------

-- normalize newline: \n or \n\r

nlChar :: TclParser Char
nlChar = char '\n'
         >> option ' ' (char '\r')
         >> return '\n'

-- parse escaped newline char

escNewline :: TclParser Char
escNewline
    = try (string "\\\n")
      >> many (oneOf " \t\r")
      >> return ' '

ws1' ::  TclParser String
ws1' = many1 (oneOf " \t\r" <|> escNewline)

wsnl1' ::  TclParser String
wsnl1'
    = many1 $ ( oneOf " \t\r"
                <|>
                nlChar
                <|>
                escNewline
              )

nl1' :: TclParser String
nl1' = toList ( nlChar <|> char ';')

noExtraChar' :: NoSubst -> TclParser ()
noExtraChar' nos
    = ( notFollowedBy $ noneOf " \t\n\r;" )
      <|>
      ( lookAhead $ getNoExtraCharParser nos >> return ())

eofP :: TclParser r -> TclParser r
eofP ps
    = do r <- ps
	 eof
	 return r

-- ------------------------------------------------------------

type NoSubst
    = [(String, TclParser String)]

getNoSubstParser :: Char -> NoSubst -> TclParser String
getNoSubstParser c =  foldr (\ (cs, p1) p2 -> if c `elem` cs then p1 else p2) mzero

getWhiteSpaceParser :: NoSubst -> TclParser String
getWhiteSpaceParser = getNoSubstParser 'w'

getNewlineParser :: NoSubst -> TclParser String
getNewlineParser = getNoSubstParser 'n'

getNoExtraCharParser :: NoSubst -> TclParser String
getNoExtraCharParser = getNoSubstParser 'x'

defaultNoSubst :: NoSubst
defaultNoSubst
    = [ ("\\", toList substBackslash)
      , ("]}", toList anyChar)
      , ("x",  mzero)
      ]

defaultNoParse :: TclParser String
defaultNoParse = toList anyChar

noSemiSubst :: NoSubst -> NoSubst
noSemiSubst = ((";", defaultNoParse) :)

noNewlineSubst :: NoSubst -> NoSubst
noNewlineSubst = (("\n", toList nlChar) :)

noCmdSeqSubst :: NoSubst -> NoSubst
noCmdSeqSubst = noSemiSubst . noNewlineSubst

inBraceSubst :: NoSubst -> NoSubst
inBraceSubst
    = ((" \t\n;$[]\"", defaultNoParse) :)
      . (("\\", substBraceBackslash) :)		-- escaped backslashes must be overread
      . (("}", mzero) :)                        -- closing brace must not be parsed

inDquoteSubst :: NoSubst -> NoSubst
inDquoteSubst
    = noNewlineSubst
      . ((" \t{}]", defaultNoParse) :)

inBracketSubst :: NoSubst -> NoSubst
inBracketSubst = ((" \t;\n\r]", mzero) :)
                 . (("x", toList $ char ']') :)	-- "] is no longer a legal chararg char but is the closing bracket
                                                -- of the surrounding [ ] pair

inProgWS :: NoSubst
inProgWS = ("w", ws1')
           : ("n", nl1')
           : defaultNoSubst

inCmdWS :: NoSubst
inCmdWS = ("w", wsnl1')
           : ("n", mzero)
           : (";", defaultNoParse)
           : defaultNoSubst

inListWS :: NoSubst
inListWS = ("w", wsnl1')
           : ("n", mzero)
           : ("$[];", defaultNoParse)
           : defaultNoSubst

-- ------------------------------------------------------------
--
-- parser lifting combinators

tLit :: TclParser String -> TclParser TclSubst
tLit = (TLit <$>)

tVar :: TclParser String -> TclParser TclSubst
tVar = (TVar <$>)

tEval :: TclParser TclProg -> TclParser TclSubst
tEval = (TEval <$>)

toList :: TclParser a -> TclParser [a]
toList = ((:[]) <$>)

-- ------------------------------------------------------------

braces :: NoSubst -> TclParser String
braces nos
    = between (char '{') (char '}') $ bracesContent nos

bracesContent :: NoSubst -> TclParser String
bracesContent nos
    = concat <$>
      many ( chars nos
             <|>
             (inBraces <$> braces nos)
           )
    where
      inBraces = ("{" ++) . (++ "}")

variableName :: NoSubst -> TclParser String
variableName _s
    = ( concat <$> many1 (many1 (letter <|> digit <|> char '_') <|> namespaceSep) )
      <|>
      ( between (char '{') (char '}') $ many1 $ noneOf "}" )
    where
      namespaceSep
	  = do c1 <- char ':'
	       cs <- many1 $ char ':'
	       return $ c1 : cs

chars :: NoSubst -> TclParser String
chars s
    = litChars
      <|>
      noSubstChars s
      <?> "ordinary char"

-- ------------------------------------------------------------

litChars :: TclParser String
litChars
    = many1 $
      noneOf $ ws ++ nls ++ vc ++ esc ++ br ++ dq ++ "]}"

noSubstChars :: NoSubst -> TclParser String
noSubstChars nos
    = concat <$> many1 (noSubstChar nos)

noSubstChar :: NoSubst -> TclParser String
noSubstChar nos
    = try $ do c <- lookAhead anyChar
               getNoSubstParser c nos

-- ------------------------------------------------------------

substBraceBackslash :: TclParser String
substBraceBackslash
    = do char '\\'
         ( option "" $ toList $ oneOf "{}" ) >>= return . ('\\' :)

substBackslash :: TclParser Char
substBackslash
    = do char '\\'
         option '\\' $
                    ( escChar
                      <|>					-- 1 to 3 digit octal number
                      ( do od <- mToN octDigit 1 3
                           return $ toEnum (((read $ "0o" ++ od) :: Int) `mod` 256)
                      )
                      <|>					-- 2 digit hex number
                      try ( do char 'x'
                               hd <- many1 hexDigit
                               let hd' = reverse . take 2 . reverse $ hd
                               return $ toEnum ((read $ "0x" ++ hd') :: Int)
                          )
                      <|>					-- 2 digit hex number
                      try ( do char 'u'
                               hd <- mToN hexDigit 1 4
                               return $ toEnum ((read $ "0x" ++ hd) :: Int)
                          )
	              <|>
	              anyChar
	            )
    where
    escChar	-- octal, hex and unicode escapes are not handled
	=  ( do
	     c <- oneOf "abfnrtv"
	     return $ read $ "\'\\" ++ [c] ++ "\'"
	   )
      
-- ------------------------------------------------------------

varArg' :: NoSubst -> TclParser TclSubst
varArg' nos
    = char '$'
      >> ( (tVar $ variableName nos)
           <|>
           (tLit $ return "$")
         )

charsArg' :: NoSubst -> TclParser TclSubst
charsArg' nos
    = tLit $ chars nos

subCmdArg' :: NoSubst -> TclParser TclSubst
subCmdArg' nos
    = tEval $
      between (char '[') (char ']') $
      aProg $ inBracketSubst nos

anArg' :: NoSubst -> TclParser TclSubst
anArg' nos
    =  charsArg' nos
       <|>
       varArg' nos
       <|>
       subCmdArg' nos

-- ------------------------------------------------------------

ordinaryArg :: NoSubst -> TclParser [TclSubst]
ordinaryArg nos
    = many1 $ anArg' nos

dquoteArg :: NoSubst -> TclParser [TclSubst]
dquoteArg nos
    = between (char '\"') (char '\"' >> noExtraChar' nos) $
      many $
      anArg' $ inDquoteSubst nos

bracketArg :: NoSubst -> TclParser [TclSubst]
bracketArg nos
    = toList $
      do a <- subCmdArg' nos
         noExtraChar' nos
         return a

braceArg  :: NoSubst -> TclParser [TclSubst]
braceArg nos
    = toList $
      do a <- tLit $ braces $ inBraceSubst nos
         noExtraChar' nos
         return a

anArg   :: NoSubst -> TclParser [TclSubst]
anArg nos
    = ordinaryArg nos
      <|>
      dquoteArg nos
      <|>
      braceArg nos
      <|>
      bracketArg nos

-- ------------------------------------------------------------

aTclProg :: TclParser TclProg
aTclProg = eofP $ aProg inProgWS

aTclList :: TclParser TclList
aTclList = TclList <$> (eofP $ anArgSeq inListWS)

aTclCmd :: TclParser TclCmd
aTclCmd = (TclCmd . concatMap _tclCmd) <$> (aCmd $ inCmdWS)

-- ------------------------------------------------------------

aProg :: NoSubst -> TclParser TclProg
aProg nos
    = TclProg <$> aCmdSeq nos

aCmdSeq ::  NoSubst -> TclParser [TclCmd]
aCmdSeq nos
    = do c1 <- aCmd nos
         cs <- option [] (getNewlineParser nos >> aCmdSeq nos)
         return $ c1 ++ cs

aCmd ::  NoSubst -> TclParser [TclCmd]
aCmd nos
    = ( option "" $ getWhiteSpaceParser nos )
      >> ( option [] $
           toList $
           ( TclCmd <$>
             ( do c1 <- TclArg <$> anArg nos
                  al <- anArgSeq nos
                  return (c1 : al)
             )
           )
         )

anArgSeq :: NoSubst -> TclParser [TclArg]
anArgSeq nos
    = ( option "" $ getWhiteSpaceParser nos )
      >> ( args' <|> return [] )
    where
      args'
          = do a1 <- TclArg <$> anArg nos
               as <- anArgSeq nos
               return $ a1 : as

-- ------------------------------------------------------------
