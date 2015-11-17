module PPL.Error where

import           Control.Monad

data Error a
    = OK a
    | Error String
      deriving (Show)

instance Functor Error where
    fmap = liftM

instance Applicative Error where
    pure  = return
    (<*>) = ap

instance Monad Error where
    (OK x)    >>= k     = k x
    (Error e) >>= _     = Error e
    return x            = OK x

