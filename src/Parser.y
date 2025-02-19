{
module Parser where
import Lexer
import AST
import           Data.Map(Map)
import qualified Data.Map as Map
}

%name parser           -- diretiva %name define o nome da função de parsing
%tokentype { Token }   -- tipo para *tokens*  (do Lexer.hs)
%error { parseError }  -- funcao para reportar erros


%token                 
-- SINTAXE NA GRAMATICA PARA TOKENS

--
num    { NUM $$ }
str    { STR $$ }
ident  { ID $$} 

-- Palavras reservadas
true   { TRUE }
false  { FALSE }
if     { IF }
else   { ELSE }
while  { WHILE }
for    { FOR }
return { RETURN }
break  { BREAK }
continue  { CONTINUE }

-- Pontuacoes
'('    { LPAREN }
')'    { RPAREN }
','    { COMMA }
';'    { SEMICOLON }
'{'    { LBRACE }
'}'    { RBRACE }
'['    { LBRACKET }
']'    { RBRACKET }

-- Operadores aritmeticos
'++' { PLUSPLUS }
'--' { MINUSMINUS }
'+'  { PLUS }
'-'  { MINUS }
'*'  { MULT }
'/'  { DIV }
'%'  { MOD }

-- Operadores relacionais
'==' { EQUAL }
'!=' { NOTEQUAL }
'<'  { LTHAN }
'<=' { LTHANEQUAL }
'>'  { GTHAN }
'>=' { GTHANEQUAL }

-- Operadores logicos
'!'  { NOT }
'&&' { AND }
'||' { OR }

-- Atribuição simples
'=' { ASSIGN } 

-- Tipos básico
--double      { DOUBLE }
int         { INT }
bool        { BOOL }
string      { STRING }

-- Funcoes de entrada e saida 
PrintInt    { PRINTINT }
ScanInt     { SCANINT }
PrintStr    { PRINTSTR }


-- PRECEDENCIAS (ver precedências no slide 20 C0-reference)
-- precedencia mais baixa para o Happy
%right '=' -- consoante a c0 reference. lá diz que é right
%left  '||'              -- c0 ref
%left  '&&'              -- c0 ref
%left  '==' '!='         -- c0 ref
%left  '>' '>=' '<' '<=' -- c0 ref
%left  '+' '-'           -- c0 ref
%left  '*' '/' '%'       -- c0 ref
%right '!' '++' '--'     -- c0 ref
%left  '(' ')' '[' ']'   -- c0 ref
-- precedencia mais alta para o Happy

%%
-- GRAMATICA

Prog
  : FuncDeclList  { GDefn $1 }

FuncDeclList
  : FunDecl FuncDeclList      { $1 : $2 }
  | FunDecl                   {[$1]}

FunDecl
  : Type ident '(' ParamList ')' Block2     {FunDecl $1 $2 $4 $6}         

ParamList
  : Param ',' ParamList { $1 : $3 }
  | Param               { [$1] }
  | {-empty-}           { [] }

Param
  : Type ident    {Param $1 $2}

Decl
  : Type IdentListV2 ';'   {distribui $1 $2 }

IdentListV2
  : ident ',' IdentListV2  {$1 : $3}
  | ident                  {[$1]}

--todo adicionar break e continue como stmt 
Stmt 
  : SimpleExp ';'    { $1 } -- Exp ';' { SimpleExp $1 }
  | AssignStm     { $1 } -- VarAccess '=' Exp ';'   { AssignStm $1 $3 }
  | IfThen        { $1 } -- if '(' Exp ')' Stmt  { IfThen $3 $5 }
  | IfThenElse    { $1 } -- if '(' Exp ')' Stmt else Stmt    { IfThenElse $3 $5 $7 }
  | Block2         { $1 } -- '{' StmtList '}'    { Block $2 }
  | While         { $1 } -- while '(' Exp ')' Stmt    { While $3 $5 }
  | For           { $1 } -- for '(' Exp ';' Exp ';' Exp ')' Stmt  { For $3 $5 $7 $9 }
  | Return        { $1 } -- return Exp ';'      { Return $2 }

SimpleExp
  : Exp           { SimpleExp $1 }

AssignStm
  : VarAccess '=' Exp ';'     { AssignStm $1 $3 }

IfThen
  : if '(' Exp ')' Stmt { IfThen $3 $5 }

IfThenElse
  : if '(' Exp ')' Stmt else Stmt   { IfThenElse $3 $5 $7 }

Block2
  : '{' DeclList  StmtList '}'   { Block2 $2 $3 }

DeclList
  : Decl DeclList { $1 ++ $2 }
  | {-empty-}     { [] }

While
     : while '(' Exp ')' Stmt          { While $3 $5 } 

For
  : for '(' AssignStm Exp ';' Exp ')' Stmt  { For $3 $4 $6 $8 }

Return
  : return Exp ';'                  { Return $2 }

StmtList
  : Stmt StmtList { $1 : $2 }
  | {-empty-}     { [] }

Exp
  : num                   { IntLiteral $1 }
  | VarAccess             { VarAccess $1 }
  | str                   { Str $1 }
  | '(' Exp ')'           { $2 }
  | VarAccess PosOp       { PosOp $1 $2 }
  | FunCall               { $1 }
  -- | ScanInt '(' ')'       { ScanInt }      --Tratamos como chamada de função
  -- | PrintInt '(' Exp ')'  { PrintInt $3 }  
  -- | PrintStr '(' str ')'  { PrintStr $3 }
  | true                  { BoolLiteralTrue }
  | false                 { BoolLiteralFalse }
  | '!' Exp               { Not $2 } 
  | Exp '+' Exp           { BinOp $1 Plus $3 } 
  | Exp '-' Exp           { BinOp $1 Minus $3 }
  | Exp '*' Exp           { BinOp $1 Mult $3 }
  | Exp '/' Exp           { BinOp $1 Div $3 }
  | Exp '%' Exp           { BinOp $1 Mod $3 }
  | Exp '==' Exp          { BinOp $1 Equal $3 }
  | Exp '!=' Exp          { BinOp $1 NotEqual $3 }
  | Exp '<' Exp           { BinOp $1 LessThan $3 }
  | Exp '<=' Exp          { BinOp $1 LessThanEqual $3 }
  | Exp '>' Exp           { BinOp $1 GreaterThan $3 }
  | Exp '>=' Exp          { BinOp $1 GreaterThanEqual $3 }
  | Exp '&&' Exp          { BinOp $1 And $3 }
  | Exp '||' Exp          { BinOp $1 Or $3 }

VarAccess
  : ident               { Id $1 }
  | ident '[' Exp ']'   { Array $1 $3 }

PosOp
  : '++'    { PlusPlus }
  | '--'    { MinusMinus }

FunCall
  : ident '(' ExpList ')'     { FunCall $1 $3 }

ExpList
  : Exp ',' ExpList     { $1 : $3 }
  | Exp                 { [$1] }
  | {-empty-}           { [] }

Type
  : int     { TypeInt }
  | bool    { TypeBool }
  | string  { TypeString }

{

parseError :: [Token] -> a
parseError toks = error ("parse error" ++ show toks) -- funcao modificada para mostrar toks
--parseError toks = error "parse error" -- funcao original

type TypeEnv = Map Ident Type

--distribui tipo (decl:xs) = undefined  
distribui tipo xs = map (\x -> (x, tipo)) xs

-- extend an environment with a list of declarations
extendEnv :: TypeEnv -> [Decl] -> TypeEnv
extendEnv env [] = env
extendEnv env ((v,t):rest) = extendEnv (Map.insert v t env) rest

} 