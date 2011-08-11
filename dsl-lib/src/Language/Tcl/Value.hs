module Language.Tcl.Value
    ( Value
    , Values
    , mkI, mkD, mkS, mkL
    , isI, isD, isS
    , selI, selD, selS, selL, selB
    , convI, convD, convS, convL, convLE

    , b2i
    , i2d, i2s
    , d2s
    , s2i
    , escapeList

    , eqarg

    , string2a
    , string2bool
    , string2double
    , string2int
    , string2integer
    , string2list
    , string2listelem

    , parseListArg
    , escapeArg
    , isBraceArg
    , isCharArg
    , inBraces

    , matchGlobPattern		-- csh glob style matching

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

import Text.Regex.Glob.String       ( match )

-- ------------------------------------------------------------

type Values
    = [Value]

data Value
    = I Integer
    | D Double
    | S String	-- a candidate for: String -> String, to speed up ++ and concat
    | L Values  -- native list value support
    | E

instance Show Value where
    show = selS

instance Eq Value where
    (==) = (==) `on` selS

instance Ord Value where
    compare = compare `on` selS

-- the monoid for string concat

instance Monoid Value where
    mempty   = value_empty
    mappend  = (.++.)

-- the "monoid" for list concat
lempty :: Value
lempty = mempty

lappend :: (MonadPlus m) => Value -> Value -> m Value
lappend = (.**.)

lconcat :: (MonadPlus m) => [Value] -> m Value
lconcat = foldM lappend lempty

-- ------------------------------------------------------------

mkI              :: Integer -> Value
mkI              = I

mkD              :: Double -> Value
mkD              = D

mkS              :: String -> Value
mkS s
    | null s     = E
    | otherwise  = S s

mkL              :: Values -> Value
mkL []           = E				-- empty list is E
mkL xs           = L xs

-- ------------------------------------------------------------
-- these operations are used only for types and conversions in arithmetic

isI              :: Value -> Bool
isI (I _)        = True
isI x            = isSingleList isI x

isD              :: Value -> Bool
isD (D _)        = True
isD x            = isSingleList isI x

isS              :: Value -> Bool
isS (S _)        = True
isS  E           = True
isS (L _)        = True
isS _            = False

isSingleList            :: (Value -> Bool) -> Value -> Bool
isSingleList p (L [v])  = p v
isSingleList _ _        = False

-- ------------------------------------------------------------
--
-- common selector functions

selI        :: MonadPlus m => Value -> m Integer
selI (I x)  = return x
selI (D d)  = return . toInteger . fromEnum $ d
selI (S s)  = string2a s
selI x      = maybeSingle selI x

selD        :: MonadPlus m => Value -> m Double
selD (D x)  = return x
selD (I x)  = return . fromIntegral $ x
selD (S s)  = string2a s
selD x      = maybeSingle selD x

selS        :: Value -> String
selS (S x)  = x
selS  E     = ""
selS (I i)  = show i
selS (D d)  = show d
selS (L l)  = intercalate " " . map (escapeList . selS) $ l

selL        :: MonadPlus m => Value -> m Values
selL (L x)  = return x
selL  E     = return []
selL (S x)  = string2list x
selL x      = return [x]

selB        :: (Functor m, MonadPlus m) => Value -> m Bool
selB x      = ( (/= 0  ) <$> selI x )
              `mplus`
              ( (/= 0.0) <$> selD x )
              `mplus`
              (string2bool $ selS x)

maybeSingle            :: MonadPlus m => (Value -> m a) -> Value -> m a
maybeSingle f (L [v])  = f v
maybeSingle _ _        = mzero

-- ------------------------------------------------------------
--
-- change the internal representation of values

convI :: (Functor m, MonadPlus m) => Value -> m Value
convI x@(I _) = return x
convI x       = mkI <$> selI x

convD :: (Functor m, MonadPlus m) => Value -> m Value
convD x@(D _) = return x
convD x       = mkD <$> selD x

convS :: (Functor m, MonadPlus m) => Value -> m Value
convS x@(S _) = return x
convS x@(E  ) = return x
convS x       = return . mkS . selS $ x

convL :: (Functor m, MonadPlus m) => Value -> m Value
convL x@(L _) = return x
convL x@(E  ) = return x
convL x       = mkL <$> selL x

convLE  :: (Functor m, MonadPlus m) => (Value -> m Value) -> Value -> m Value
convLE cv l
    = convL l >>= selL >>= mapM cv >>= (return . mkL) 

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

eqarg                  :: (MonadPlus m) => Value -> Value -> m (Value, Value)
eqarg x@(I _) y@(I _)  = return (x, y)
eqarg x@(D _) y@(D _)  = return (x, y)
eqarg x@(S _) y@(S _)  = return (x, y)
eqarg x@(E  ) y@(E  )  = return (x, y)

eqarg x@(S _) y@(E  )  = return (x, y)
eqarg x@(E  ) y@(S _)  = return (x, y)

eqarg _       _        = mzero

-- ------------------------------------------------------------

i2d :: (MonadPlus m) => Value -> m Value
i2d (I i) = return . D . fromIntegral $ i
i2d _     = mzero

i2s :: (MonadPlus m) => Value -> m Value
i2s (I i) = return . S . show $ i
i2s _     = mzero

d2s :: (MonadPlus m) => Value -> m Value
d2s (D x) = return . S . show $ x
d2s _     = mzero

b2i :: Bool -> Value
b2i =  I . toInteger . fromEnum

s2i :: (Functor m, MonadPlus m) => Value -> m Value
s2i v
    | isS v     = mkI <$> selI v
    | otherwise = mzero

-- ------------------------------------------------------------
--
-- string conversions

string2bool :: (MonadPlus m) => String -> m Bool
string2bool
    = maybe mzero return
      . flip lookup (zip ["true", "false", "yes", "no" ]
                         [True,   False,   True,  False]
                    )

string2list :: (MonadPlus m) => String -> m Values
string2list
    = maybe mzero return . fmap (map mkS) . parseListArg

string2int      :: (MonadPlus m) => String -> m Int
string2int      =string2a

string2integer  :: (MonadPlus m) => String -> m Integer
string2integer  =string2a

string2double   :: (MonadPlus m) => String -> m Double
string2double   =string2a

string2a :: (MonadPlus m, Read a) => String -> m a
string2a s
    = case reads s of
        [] -> mzero
        (v, rest) : _
            -> if all isSpace rest
               then return v
               else mzero

-- add extra backslashes or braces to build a list value

escapeList :: String -> String
escapeList s
    | null s       = inBraces ""
    | isCharArg  s = s
    | isBraceArg s = inBraces s
    | otherwise    = escapeArg s

-- ------------------------------------------------------------

-- string concatenation on values

(.++.)                   :: Value -> Value -> Value
E         .++. v2        = v2
v1        .++.  E        = v1
(S s1)    .++. (S s2)    = mkS $      s1 ++      s2
v1        .++. v2        = mkS $ selS v1 ++ selS v2

-- list concatenation on values

(.**.)                   :: (MonadPlus m) => Value -> Value -> m Value
E         .**. v2@(L _ ) = return v2
v1@(L _)  .**.     E     = return v1
(L l1)    .**. (L l2)    = return .           mkL $ l1 ++ l2
v1        .**. v2        = liftM2 (\ l1 l2 -> mkL $ l1 ++ l2) (selL v1) (selL v2)

-- ------------------------------------------------------------

string2listelem :: String -> String
string2listelem s
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

matchGlobPattern :: String -> String -> Bool
matchGlobPattern = match

-- ------------------------------------------------------------
