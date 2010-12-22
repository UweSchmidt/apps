{
{-# OPTIONS_GHC -w #-}

module PPL.Parser where

import PPL.Symbol
import PPL.AbstractSyntax

}

%name parser

%tokentype     { Symbol }

%token
        ':='    { (Assign, _) }
        ':'     { (Colon, _) }
        ';'     { (Semicolon, _) }
        ','     { (Comma, _) }
        '.'     { (Dot, _) }
        '('     { (LPar, _) }
        ')'     { (RPar, _) }
        '['     { (LBr, _) }
        ']'     { (RBr, _) }
        '+'     { (PlusOp, $$) }
        '-'     { (MinusOp, $$) }
        '*'     { (MultOp, $$) }
        '/'     { (DivOp, $$) }
        '='     { (EqOp, $$) }
        '/='    { (NeOp, $$) }
        '>='    { (GeOp, $$) }
        '>'     { (GrOp, $$) }
        '<='    { (LeOp, $$) }
        '<'     { (LtOp, $$) }
        and     { (AndSy, $$) }
        or      { (OrSy, $$) }
        xor     { (XorSy, $$) }
        not     { (NotSy, $$) }
        '<=>'   { (EquivOp, $$) }
        '=>'    { (ImplOp, $$) }
        div     { (DivSy, $$) }
        mod     { (ModSy, $$) }
        min     { (MinSy, $$) }
        max     { (MaxSy, $$) }
        int     { (IntConst, $$) }
        bool    { (BoolConst, $$) }
        string  { (StringConst, $$) }
        float   { (FloatConst, $$) }
        id      { (IdentSy, $$) }
        'if'    { (IfSy, _) }
        'then'  { (ThenSy, _) }
        'else'  { (ElseSy, _) }
        elseif  { (ElseIfSy, _) }
        while   { (WhileSy, _) }
        'do'    { (DoSy, _) }
        repeat  { (RepeatSy, _) }
        until   { (UntilSy, _) }
        'of'    { (OfSy, _) }
        var     { (VarSy, _) }
        function        { (FunctionSy, _) }
        procedure       { (ProcedureSy, _) }
        begin   { (BeginSy, _) }
        end     { (EndSy, _) }
        endif   { (EndIfSy, _) }
        endwhile        { (EndWhileSy, _) }
        return  { (ReturnSy, _) }
        eof     { (Eof, _) }
        tint    { (IntSy, _) }
        tfloat  { (FloatSy, _) }
        tbool   { (BoolSy, _) }
        tpic    { (PicSy, _) }
        tstring { (StringSy, _) }
        tlist   { (ListSy, _) }

%left           'else'
%left           '<=>'
%nonassoc       '=>'
%left           and or xor
%left           not
%nonassoc       '=' '/=' '>=' '>' '<=' '<'
%left           '+' '-' min max
%left           '*' '/' div mod
%right          UNop
%left           '.' ']'

%%

Prog    :: { Program }
Prog    : GDeclL Stmt eof       { Program $1 $2 }

Stmt    :: { Stmt }
Stmt    : Block                 { $1 }
        | IfStmt                { $1 }
        | WhileStmt             { $1 }
        | RepeatStmt            { $1 }
        | Assignment            { $1 }
        | ProcCall              { $1 }

Block           :: { Stmt }
Block           : begin DeSt0 end
                                { Block $2 }


IfStmt          :: { Stmt }
IfStmt          : 'if' Expr 'then' StmtList ElsePart endif
                                { If $2 (Block $4) $5 }


ElsePart        :: { Stmt }
ElsePart        :
                                { Block [] }
                | 'else' StmtList
                                { Block $2 }
                | elseif Expr 'then' StmtList ElsePart
                                { If $2 (Block $4) $5 }


WhileStmt       :: { Stmt }
WhileStmt       : while Expr 'do' StmtList endwhile
                                { While $2 (Block $4) }


RepeatStmt      :: { Stmt }
RepeatStmt      : repeat StmtList until Expr
                                { Repeat (Block $2) $4 }


Assignment      :: { Stmt }
Assignment      : Varl ':=' Exprl
                                { Assignment $1 $3 }


ProcCall        :: { Stmt }
ProcCall        : Call          { ProcCall $1 }

StmtList        :: { [Stmt] }
StmtList        :               { [] }
                | StmtL
                                { $1 }

DeSt0   :: { [Stmt] }
DeSt0   :                       { [] }
        | DeSt1                 { $1 }

DeSt1   :: { [Stmt] }
DeSt1   : Decl                  { $1 }
        | Decl ';' DeSt1        { $1 ++ $3 }
        | StmtL                 { $1 }

StmtL   :: { [Stmt] }
StmtL   : Stmt                  { [$1] }
        | StmtL ';' Stmt        { $1 ++ [$3] }

Decl    :: { [Stmt] }
Decl    : var Varl ':' Type ':=' Exprl
                                { map (\id -> Decl id $4) $2 ++ [Assignment $2 $6] }
        | var Varl ':' Type
                                { map (\id -> Decl id $4) $2 }

GDeclL  :: { [GlobDecl] }
GDeclL  :                       { [] }
        | GDecl ';' GDeclL      { $1 ++ $3 }

GDecl   : Decl                  { $1 }
        | FctDecl               { [$1] }

FctDecl :: { Stmt }
FctDecl : function Id '(' FormalParList ')' ':' Type Expr
                                { FctDecl $2 $4 $7 $8 }
        | procedure Id '(' FormalParList ')' Stmt
                                { ProcDecl $2 $4 $6 }

FormalParList   :: { [ParamDecl] }
FormalParList   :               { [] }
                | FormalParL1   { $1 }

FormalParL1     :: { [ParamDecl] }
FormalParL1     : FormalPar     { $1 }
                | FormalParL1 ';' FormalPar
                                { $1 ++ $3 }

FormalPar       :: { [ParamDecl] }
FormalPar       : Varl ':' Type { map (\id -> Decl id $3) $1 }

Type    :: { Type }
Type    : tint                  { IntType }
        | tfloat                { FloatType }
        | tbool                 { BoolType }
        | tpic                  { PictureType }
        | tstring               { StringType }
        | tlist 'of' Type       { ListType $3 }

Varl    :: { [Var] }
Varl    : Id                    { [$1] }
        | Id ',' Varl           { $1 : $3 }

Id      :: { Expr }
Id      : id                    { Ident $1 }

Exprl   :: { [Expr] }
Exprl   : Expr                  { [$1] }
        | Expr ',' Exprl        { $1 : $3 }

Expr    :: { Expr }
Expr    : SExpr                 { $1 }
        | '[' ListC             { $2 }
        | Call                  { $1 }
        | BlExpr                { $1 }
        | 'if' Expr 'then' Expr 'else' Expr
                                { Call "if" [$2,$4,$6] }
        | not Expr %prec not    { Call $1   [$2] }
        | '-' Expr %prec UNop   { Call "-u" [$2] }
        | '+' Expr %prec UNop   { Call "+u" [$2] }
        | Expr and  Expr        { Call $2 [$1, $3] }
        | Expr or   Expr        { Call $2 [$1, $3] }
        | Expr xor  Expr        { Call $2 [$1, $3] }
        | Expr '=>'  Expr       { Call $2 [$1, $3] }
        | Expr '<=>'  Expr      { Call $2 [$1, $3] }
        | Expr '+'  Expr        { Call $2 [$1, $3] }
        | Expr '-'  Expr        { Call $2 [$1, $3] }
        | Expr '*'  Expr        { Call $2 [$1, $3] }
        | Expr '/'  Expr        { Call $2 [$1, $3] }
        | Expr div  Expr        { Call $2 [$1, $3] }
        | Expr mod  Expr        { Call $2 [$1, $3] }
        | Expr min  Expr        { Call $2 [$1, $3] }
        | Expr max  Expr        { Call $2 [$1, $3] }
        | Expr '='  Expr        { Call $2 [$1, $3] }
        | Expr '/=' Expr        { Call $2 [$1, $3] }
        | Expr '>=' Expr        { Call $2 [$1, $3] }
        | Expr '>'  Expr        { Call $2 [$1, $3] }
        | Expr '<=' Expr        { Call $2 [$1, $3] }
        | Expr '<'  Expr        { Call $2 [$1, $3] }

SExpr   :: { Expr }
SExpr   : int                   { IntVal ((read $1)::Int) }
        | bool                  { BoolVal (if $1 == "false"
                                           then False
                                           else True) }
        | string                { StringVal $1 }
        | float                 { FloatVal ((read $1)::Double) }
        | Id                    { $1 }
        | '(' Expr ')'          { $2 }

BlExpr  :: { Expr }
BlExpr  : begin DSE end         { $2 }

DSE     : Decl ';' DSE          { let BlockExpr b3 e3 = $3 in BlockExpr ($1 ++ b3) e3 }
        | DSE1                  { $1 }

DSE1    : Stmt ';' DSE1         { let BlockExpr b3 e3 = $3 in BlockExpr ($1 : b3) e3 }
        | return Expr           { BlockExpr [] $2 }

ListC   :: { Expr }
ListC   : ']'                   { EmptyList }
        | Exprl ']'             { foldr (\e1 e2 ->
                                            Call "cons" [e2,e1])
                                        EmptyList
                                        $1
                                }

Call    :: { Expr }
Call    : id '(' Params ')'     { Call $1 $3 }
        | SExpr Select          { let (fct, pl) = $2 in Call fct ($1 : pl)
                                }
        | Call Select           { let (fct, pl) = $2 in Call fct ($1 : pl)
                                }

Select  :: { (String, [Expr]) }
Select  : '.' id ParList        { ($2, $3) }
        | '[' Expr ']'          { ("[.]", [$2]) }

ParList :: { [Expr] }
ParList :                       { [] }
        | '(' Params ')'        { $2 }


Params  :: { [Expr] }
Params  :                       { [] }
        | Expr                  { [$1] }
        | Expr ',' Params       { $1 : $3 }


{

happyError   :: [Symbol] -> a
happyError e =  error ( "Syntax error before:\n"
                        ++ genErrMes e
                        ++ " ..."
                      )

genErrMes e
    = (concat . map (\ (_, txt) -> txt ++ " ")) (take 10 e)

}
