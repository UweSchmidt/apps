module Language.Tcl.Value
    ( Value
    , mkI, mkD, mkS, mkL
    , isI, isD, isS
    , selI, selD, selS, selL

    , b2i
    , i2d, i2s
    , d2s
    , s2i, s2b, s2l
    , v2b, v2s, v2i, v2l

    , eqarg

    , readValue

    , stringToList
    , parseListArg
    , escapeArg
    , isBraceArg
    , isCharArg
    , inBraces

    , value_empty, value_0, value_1, value_42, value_true, value_false

    , lempty, lappend, lconcat
    )
where

import Control.Monad
import Control.Applicative

import Data.Char                    ( isSpace )
import Data.Function       	    ( on )
import Data.List                    ( intercalate )
import Data.Monoid

import Language.Tcl.Parser	    ( isBraceArg
                                    , isCharArg
                                    , parseListArg
                                    )

-- ------------------------------------------------------------

type Values
    = [Value]

data Value
    = I Integer
    | D Double
    | S String	-- a candidate for: String -> String, to speed up ++ and concat
--  | L Values  -- native list value support
    | E

instance Show Value where
    show = v2s

instance Eq Value where
    (==) = (==) `on` v2s

instance Ord Value where
    compare = compare `on` v2s

instance Monoid Value where
    mempty   = value_empty
    mappend  = (.++.)

lempty :: Value
lempty = mempty

lappend :: Value -> Value -> Value
lappend = (.**.)

lconcat :: [Value] -> Value
lconcat = foldr lappend lempty

-- ------------------------------------------------------------

mkI              :: Integer -> Value
mkI              = I

mkD              :: Double -> Value
mkD              = D

mkS              :: String -> Value
mkS s
    | null s     = E
    | otherwise  = S s

mkL              :: Values -> Value		-- lists are stil represented as strings
{-
mkL []           = E				-- empty list is E
mkL [x]          = x                            -- single list is a single list escaped value
mkL xs           = L xs
-}
mkL              = mkS . intercalate " " . map (v2s . v2l)

-- these operations are used for arithmetic
-- only for arithmetic

isI              :: Value -> Bool
isI (I _)        = True
isI _            = False

isD              :: Value -> Bool
isD (D _)        = True
isD _            = False

isS              :: Value -> Bool -- all other types in arithmetic are processed as strings
isS (S _)        = True
isS E            = True
{-
isS (L _)        = True
-}
isS _            = False

-- ------------------------------------------------------------
--
-- common selector functions

selI             :: MonadPlus m => Value -> m Integer
selI (I x)       = return x
selI _           = mzero

selD             :: MonadPlus m => Value -> m Double
selD (D x)       = return x
selD _           = mzero

selS             :: MonadPlus m => Value -> m String
selS (S x)       = return x
selS E           = return ""
{-
selS x           = return . v2s $ x
-}
selS _           = mzero

selL             :: MonadPlus m => Value -> m Values
{-
selL (L x)       = return x
selL E           = return []
selL v           = return [v]
-}
selL             = maybe mzero return . fmap (map mkS) . parseListArg . v2s

-- ------------------------------------------------------------

value_empty  :: Value
value_empty  = E

value_1      :: Value
value_1      = mkI 1

value_0      :: Value
value_0      = mkI 0

value_42     :: Value
value_42     = mkI 42

value_true   :: Value
value_true   = value_1

value_false  :: Value
value_false  = value_0

-- ------------------------------------------------------------

eqarg :: Value -> Value -> Maybe (Value, Value)
eqarg x@(I _) y@(I _) = return (x, y)
eqarg x@(D _) y@(D _) = return (x, y)
eqarg x@(S _) y@(S _) = return (x, y)
eqarg x@(E  ) y@(E  ) = return (x, y)

eqarg x@(S _) y@(E  ) = return (x, y)
eqarg x@(E  ) y@(S _) = return (x, y)

eqarg _       _       = mzero

s2i :: Value -> Maybe Value
s2i (S s) = fmap (I . toInteger . fromEnum) . s2b $ s
s2i _     = mzero

i2d :: Value -> Maybe Value
i2d (I i) = return . D . fromIntegral $ i
i2d _     = mzero

i2s :: Value -> Maybe Value
i2s (I i) = return . S . show $ i
i2s _     = mzero

d2s :: Value -> Maybe Value
d2s (D x) = return . S . show $ x
d2s _     = mzero

b2i :: Bool -> Value
b2i =  I . toInteger . fromEnum

s2b :: String -> Maybe Bool
s2b = flip lookup $ zip ["true", "false", "yes", "no"] [True, False, True, False]

s2l :: String -> Maybe Value
s2l = mkL . fmap (map mkS) . parseListArg

v2b :: Value -> Maybe Bool
v2b x = ( (/= 0  ) <$> selI x )
        `mplus`
        ( (/= 0.0) <$> selD x )
        `mplus`
        ( selS x >>= s2b )

v2s :: Value -> String
v2s (S s) = s
v2s (I i) = show i
{-
v2s (L l) = intercalate " " . map (v2s . v2l)
-}
v2s (D d) = show d
v2s  E    = ""

v2i :: Value -> Maybe Integer
v2i (I i) = return i
v2i (D d) = return . toInteger . fromEnum $ d
v2i (S s) = readValue s
v2i  E    = mzero

readValue :: (Read a) => String -> Maybe a
readValue s
    = case reads s of
        [] -> mzero
        (v, rest) : _
            -> if all isSpace rest
               then return v
               else mzero

-- add extra backslashes or braces to build a list value
-- only strings must be quoted

v2l :: Value -> Value
v2l v@(S s)
--  | null       s = mkS "{}"	    -- does not longer occur, empty string is E
    | isCharArg  s = v
    | isBraceArg s = mkS $ inBraces s
    | otherwise    = mkS $ escapeArg s
v2l E              = mkS $ "{}"
v2l v              = v

-- string concatenation on values

(.++.)               :: Value -> Value -> Value
E       .++. v2      = v2
v1      .++. E       = v1
(S s1)  .++. (S s2)  = mkS $     s1 ++     s2
v1      .++. v2      = mkS $ v2s v1 ++ v2s v2

-- list concatenation on values

(.**.)              :: Value -> Value -> Value
E       .**. v2      = v2
v1      .**. E       = v1
{-
(L l1)  .**. (L l2)  = mkL $     l1 ++     l2
(L l1)  .**. v2      = mkL $     l1 ++    [v2]
v1      .**. (L l2)  = mkL $ v1 : l2
v1      .**. v2      = mkL $ [v1, v2]
-}
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
