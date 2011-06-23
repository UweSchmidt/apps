module Language.Tcl.Value
    ( Value
    , mkI, mkD, mkS
    , isI, isD, isS
    , selI, selD, selS

    , b2i
    , i2d, i2s
    , d2s
    , s2i, s2b
    , v2b, v2s, v2i, v2l

    , eqarg

    , readValue

    , value_empty, value_0, value_1, value_42, value_true, value_false
    )
where

import Control.Monad
import Control.Applicative

import Data.Char                    ( isSpace )
import Data.Function       	    ( on )
import Data.Monoid

import Language.Tcl.QuoteListValue

-- ------------------------------------------------------------

data Value
    = I Integer
    | D Double
    | S String	-- a candidate for: String -> String, to speed up ++ and concat

instance Show Value where
    show = v2s

instance Eq Value where
    (==) = (==) `on` v2s

instance Ord Value where
    compare = compare `on` v2s

instance Monoid Value where
    mempty        = value_empty
    mappend x1 x2 = mkS $ v2s x1 ++ v2s x2	-- this can be optimized with String -> String
 
-- ------------------------------------------------------------

mkI         :: Integer -> Value
mkI         = I

mkD         :: Double -> Value
mkD         = D

mkS         :: String -> Value
mkS         = S

isI         :: Value -> Bool
isI (I _)   = True
isI _       = False

isD         :: Value -> Bool
isD (D _)   = True
isD _       = False

isS         :: Value -> Bool
isS (S _)   = True
isS _       = False

selI        :: MonadPlus m => Value -> m Integer
selI (I x)  = return x
selI _      = mzero

selD        :: MonadPlus m => Value -> m Double
selD (D x)  = return x
selD _      = mzero

selS        :: MonadPlus m => Value -> m String
selS (S x)  = return x
selS _      = mzero

-- ------------------------------------------------------------

value_empty  :: Value
value_empty  = mkS ""

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

v2b :: Value -> Maybe Bool
v2b x = ( (/= 0  ) <$> selI x )
        `mplus`
        ( (/= 0.0) <$> selD x )
        `mplus`
        ( selS x >>= s2b )

v2s :: Value -> String
v2s (S s) = s
v2s (I i) = show i
v2s (D d) = show d

v2i :: Value -> Maybe Integer
v2i (I i) = return i
v2i (D d) = return . toInteger . fromEnum $ d
v2i (S s) = readValue s

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
    | null       s = mkS "{}"
    | isCharArg  s = v
    | isBraceArg s = mkS $ inBraces s
    | otherwise    = mkS $ escapeArg s
v2l v
    = v

-- ------------------------------------------------------------
