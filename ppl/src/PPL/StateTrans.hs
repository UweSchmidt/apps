-- $Id: StateTrans.hs,v 1.2 2001/03/01 14:31:53 uwe Exp $

module PPL.StateTrans where

newtype StateTrans s a = ST ( s -> (s, Maybe a))

-- state monad with error handling
-- in case of an error, the state remains
-- as it is and Nothing is returned as value
-- else execution continues

instance Monad (StateTrans s) where
    (ST p) >>= k
	= ST (\s0 -> let
		     (s1, r0)	= p s0
		     in
		     case r0 of
		     Just v -> let
                               (ST q) = k v
			       in
			       q s1
		     Nothing -> (s1, Nothing)
	     )
    return v
	= ST (\s -> (s, Just v))

