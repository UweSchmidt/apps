module PPL.AbstractSyntax where

data Program
    = Program [GlobDecl] Stmt
      deriving (Eq, Show)

data Stmt
    = Assignment        [Var] [Expr]
    | Decl              Var Type
    | FctDecl           FctName [ParamDecl] ResType FctBody
    | ProcDecl          FctName [ParamDecl] Stmt
    | ProcCall          Expr
    | Block             [Stmt]
    | While             Expr Stmt
    | Repeat            Stmt Expr
    | If                Expr Stmt Stmt
      deriving (Eq, Show)

data Expr
    = UndefVal
    | IntVal    Int
    | BoolVal   Bool
    | FloatVal  Double
    | StringVal String
    | EmptyList
    | Ident     String
    | Call      String [Expr]
    | Opr       String [AttrTree]
    | BlockExpr [Stmt] Expr
      deriving (Eq, Show)

data Type
    = UnknownType
    | AnyType
    | VoidType
    | IntType
    | BoolType
    | FloatType
    | PictureType
    | StringType
    | ListType  Type
    | FctType   Type [Type]
      deriving (Eq, Show)

type Var        = Expr
type FctName    = Expr
type ParamDecl  = Stmt
type ResType    = Type
type FctBody    = Expr
type GlobDecl   = Stmt

type AttrTree = (Expr, Type)

