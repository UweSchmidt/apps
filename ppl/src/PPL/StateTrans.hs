module PPL.StateTrans where

import Control.Monad

newtype StateTrans s a = ST ( s -> (s, Maybe a))

-- state monad with error handling
-- in case of an error, the state remains
-- as it is and Nothing is returned as value
-- else execution continues

instance Monad (StateTrans s) where
    (ST p) >>= k
        = ST (\s0 -> let
                     (s1, r0)   = p s0
                     in
                     case r0 of
                     Just v -> let
                               (ST q) = k v
                               in
                               q s1
                     Nothing -> (s1, Nothing)
             )

instance Applicative (StateTrans s) where
  pure v = ST (\s -> (s, Just v))
  (<*>)  = ap

instance Functor (StateTrans s) where
  fmap = liftM
