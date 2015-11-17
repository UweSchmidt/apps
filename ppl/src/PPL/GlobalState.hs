module PPL.GlobalState where

import           Control.Monad

import           PPL.Instructions

type State      = [(Attr, Value)]

type Attr       = String

data Value
    = LabCnt    Int
    | Allocator Alloc
    | AddrCnt   Int
    | AddrList  AddrList
    | Unknown
--    deriving Show

type Alloc      = Int -> Address

type AddrList   = [(String, Address)]

newtype GS v    = GS (State -> (State, v))

-- the state monad

instance Functor GS where
    fmap = liftM

instance Applicative GS where
    pure  = return
    (<*>) = ap

instance Monad GS where
    return v
        = GS (\s -> (s,v))

    GS sm0 >>= fsm1
        = GS (\s0 ->
              let
              (s1,v1) = sm0 s0
              GS sm1  = fsm1 v1
              (s2,v2) = sm1 s1
              in (s2, v2)
             )

-- the initial state

initialState :: State
initialState = []

lookupAttr      :: Attr -> State -> Value
lookupAttr a s
    = case lookup a s of
      Nothing   -> Unknown
      Just val  -> val

updateValue     :: Attr -> Value -> State -> State
updateValue a v s
    = (a, v) : filter (\(a', _) -> a' /= a) s

-- the state access functions

lookupState :: Attr -> GS Value
lookupState a
    = GS (\s -> (s, lookupAttr a s))

updateState :: Attr -> Value -> GS ()
updateState a v
    = GS (\s -> (updateValue a v s, ()))

