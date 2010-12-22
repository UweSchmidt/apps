-- $Id: Error.hs,v 1.4 2001/12/11 22:30:14 uwe Exp $

module PPL.Error where

data Error a
    = OK a
    | Error String
      deriving (Show)

instance Monad Error where
    (OK x)    >>= k	= k x
    (Error e) >>= _	= Error e
    return x		= OK x

