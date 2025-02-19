module AST where 

type Ident = String

data Prog = GDefn [FunDecl]
          deriving(Show)

data Stmt =
          AssignStm VarAccess Exp     -- x = 1; y[0]=1;
          | SimpleExp Exp               -- 1+2; , x; , true;
          | IfThen Exp Stmt             -- if (x<1) x=1;
          | IfThenElse Exp Stmt Stmt    -- if (x<1) x=1; else x=2;
          | While Exp Stmt              -- While (x>1) {x=1;}
          | For Stmt Exp Exp Stmt        -- For (x=0; x>1; x++) {x=1;}
          | Block [Stmt]                -- {(...)}
          | Block2 [Decl] [Stmt]                -- {(...)}
          | Return Exp                  -- return 1;
          deriving(Show)

-- types
data Type = TypeInt       --int
          | TypeBool      --bool
          | TypeString    --string
          deriving (Show,Eq)

-- variable declarations
type Decl = (Ident, Type)           -- variable, type
-- type TypeEnv = Map Ident Type -- ambiente tabela de simbolos

-- function declaration
data FunDecl = FunDecl Type Ident [Param] Stmt  --(IMPORTANTE) este Stmt é forçado a ser Block2 no Parser!!!
             deriving (Show)

-- function parameters
data Param = Param Type Ident
            deriving (Show)

-- expressions
data Exp = IntLiteral Int      -- Declaramos um novo tipo algébrico para árvores sintáticas
         | BoolLiteralTrue
         | BoolLiteralFalse
         | Str String
         | VarAccess VarAccess
         | PosOp VarAccess PosOp
         | BinOp Exp BinOp Exp
         | Not Exp
         | FunCall Ident [Exp]
         | Decl (Ident, Type)
         deriving (Show)

-- Variaveis podem ser simples ou arrays com indices
data VarAccess = Id Ident
               | Array Ident Exp
               deriving (Show)

-- Operatores Pos
data PosOp = PlusPlus
           | MinusMinus
           deriving (Show)

-- Operatores Aritmeticos
data BinOp = Plus
           | Minus
           | Mult 
           | Div
           | Mod
           | Equal
           | NotEqual
           | LessThan
           | GreaterThan
           | LessThanEqual
           | GreaterThanEqual
           | And
           | Or
           deriving (Eq,Show)

-- function definition (código intermédio)
data FunDef = FunDef Ident [Ident] [Ident] [Stmt]
            --        fun   (args)   locals  body
            deriving (Show)
