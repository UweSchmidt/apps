module TclParser
where

import Data.Char
import Data.List
-- import Data.Monoid

import Text.Parsec

-- ------------------------------------------------------------

type TclParser = Parsec String ()

newtype TclProg
    = TclProg { _tclProg :: [TclCmd]}
      deriving (Show)

newtype TclCmd
    = TclCmd { _tclCmd :: [TclArg]}	-- list contains at least 1 arg
      deriving (Show)

newtype TclList
    = TclList { _tclList :: [TclArg]}  -- all args do contain only TLit's
      deriving (Show)

newtype TclArg
    = TclArg { _tclArg :: [TclSubst]}
      deriving (Show)

data TclSubst
    = TLit  String
    | TVar  String
    | TEval TclProg
      deriving (Show)

{-
instance Show TclProg where
    show (TclProg cl) = "\n" ++ concatMap show cl

instance Show TclCmd where
    show (TclCmd (c : al)) = show c ++ concatMap ((" " ++) . show) al ++ "\n"

instance Show TclArg where
    show (TclArg s) = concatMap show s

instance Show TclSubst where
    show (TLit  s)
	=  concatMap escapeChar s
	   where
	   escapeChar c
	       | isAlphaNum c
		   = [c]
	       | c `elem` " {}[]$\\"
                   = "\\" ++ [c]
	       | otherwise
		   = reverse . tail . reverse . tail . show $ c
    show (TVar  v)
	= "$" ++ escapeVarName v
	  where
	  escapeVarName s
	      |  all (\ c -> isAlphaNum c || c == '_' || c == ':') s
                  = s
	      | otherwise
		  = "{" ++ s ++ "}"

    Show (TEval p)
	= "[" ++ show p ++ "]"
-}

-- ------------------------------------------------------------

showTclList	:: TclList -> String
showTclList
    = showTclArgs . _tclList

showTclArgs	:: [TclArg] -> String
showTclArgs
    = intercalate " " . map showTclArg 

showTclArg	:: TclArg -> String
showTclArg (TclArg a)
    | null s       = inBraces  s
    | isCharArg  s =           s	-- try to avoid escapes and braces
    | isBraceArg s = inBraces  s	-- try to avoid escapes
    | otherwise    = escapeArg s	-- some chars must be escaped
    where
      s = concatMap showTclSubst a

showTclSubst	:: TclSubst -> String
showTclSubst (TLit s)
    = s
showTclSubst (TVar n)
    = "$" ++ n
showTclSubst (TEval _pg)
    = error "showTclSubst for TEval not implemented"

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
--
-- the main Tcl parsers

tclProg :: TclParser TclProg
tclProg
    = tprog

tclList :: TclParser TclList
tclList
    = do
      l <- largs
      return $ TclList l

-- ------------------------------------------------------------

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

largs :: TclParser [TclArg]
largs
    = targs' wsnl1 parserZero "" ""

targs' :: TclParser () ->		-- ^ whitespace parser, newline is included for list parser
          TclParser TclSubst ->         -- ^ variable and command substituion parser, mzero for list parser
          String ->                     -- ^ variable and command chars $ and [, or "" for list parser
          String ->                     -- ^ command follow chars
          TclParser [TclArg]
targs' tws tsubst vc' na
    = option [] $
      do
      tws
      ( targs1 <|> return [] )
    where
    targs1
	= do
	  a  <- targ'      tsubst vc' na
	  as <- targs' tws tsubst vc' na
	  return $ a : as

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

targ :: String -> TclParser TclArg
targ
    = targ' (tvar <|> tbracket) vc

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
        <|>
        ( do					-- 1 to 3 digit octal number
          od <- mToN (oneOf "01234567") 1 3
          return $ TLit $ toEnum (((read $ "0o" ++ od) :: Int) `mod` 256) : ""
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

noExtraChar :: String -> TclParser ()
noExtraChar na
    = notFollowedBy $ noneOf $ na ++ ws ++ nls

ws1 ::  TclParser ()
ws1 = many1 (oneOf " \t\r" <|> bescNL)
      >> return ()

ws0 ::  TclParser ()
ws0 = option () ws1

wsnl1 ::  TclParser ()
wsnl1
    = do
      many1 $ ( oneOf " \t\r" >> return () )
              <|>
              nl'
              <|>
              ( bescNL >> return () )
      return ()

nl' :: TclParser ()
nl' = char '\n'
      >> option ' ' (char '\r')
      >> return ()

nl1 :: TclParser ()
nl1 = nl'
      <|>
      ( char ';' >> return () )

nl0 ::  TclParser ()
nl0 = option () nl1

eofP :: TclParser r -> TclParser r
eofP ps
    = do r <- ps
	 eof
	 return r

-- ------------------------------------------------------------

p ps s = parse (eofP ps) "" s
