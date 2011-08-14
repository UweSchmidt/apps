module Language.Tcl.AbstractSyntax
    ( TclProg(..)
    , TclCmd(..)
    , TclList(..)
    , TclArg(..)
    , TclSubst(..)
    )
where

-- ------------------------------------------------------------

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
    = TLit  { _tlit  :: String  }
    | TVar  { _tvar  :: String
            , _tix   :: Maybe [TclSubst]
            }
    | TEval { _tprog :: TclProg }
      deriving (Show)

-- ------------------------------------------------------------
