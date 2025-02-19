{-
  Basic translation of expressions into 3-Address Intermediate Code

  Version 1: Explicit passing of temporary and name supply
  2023
-}
module CodeGen where

import           AST
import           IR
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State (State)
import qualified Control.Monad.State as State

-- symbol table mapping variables to temporaries
type Table = Map Ident Temp

-- the "supply" for temporaries and labels
-- just two counters for names already used
type Supply = (Int,Int)  


-- starting values for temporary and label counters
initialSupply :: Supply
initialSupply = (0, 0)

-- run a code generation action with initial supply
runCodeGen :: State Supply [Instr] -> [Instr]
runCodeGen gen = State.evalState gen initialSupply

-- get a new temporary
newTemp :: State Supply Temp
newTemp 
  = do (temps,labels) <- State.get
       State.put (temps+1, labels)
       return ("t"++show temps)

-- get several temporaries
newTemps :: Int -> State Supply [Temp]
newTemps n | n > 0 = do
               t <- newTemp
               ts <- newTemps (n-1)
               return (t:ts)
            | n == 0 = return []    --todo confirmar se isto é valido


-- get a new label
newLabel :: State Supply Label 
newLabel
  = do (temps,labels) <- State.get
       State.put (temps, labels+1)
       return ("L"++show labels)


-- give back `n' temporaries for reuse
reuseTemps :: Int -> State Supply ()
reuseTemps n
  = do (temps, labels) <- State.get
       State.put (temps-n, labels)  

---------------------------------------------------------------------------

-- translate an expression
transExpr :: Exp -> Table -> Temp -> State Supply [Instr]
transExpr (IntLiteral n) tabl dest
  = return [MOVEI dest n]

transExpr (BoolLiteralFalse) tabl dest 
  = return [MOVEI dest 0]

transExpr (BoolLiteralTrue) tabl dest
  = return [MOVEI dest 1]

transExpr (Str s) tabl dest
  = return [MOVEIS dest s]

transExpr (VarAccess (Id x)) tabl dest
  = case Map.lookup x tabl of
      Just temp -> return [MOVE dest temp]
      Nothing -> error "undefined variable " 

-- !TO DO
transExpr (VarAccess (Array id exp)) tabl dest 
  = undefined

transExpr (BinOp e1 op e2 ) tabl dest
  = do temp1 <- newTemp 
       temp2 <- newTemp 
       code1 <- transExpr e1 tabl temp1 
       code2 <- transExpr e2 tabl temp2
       reuseTemps 2
       return (code1 ++ code2 ++ [OP op dest temp1 temp2])

transExpr (PosOp e1 PlusPlus) tabl dest 
  = undefined
--   = do temp1 <- newTemp 
--        code1 <- transExpr (VarAccess e1) tabl temp1 
--        reuseTemps 1
--        return (code1 ++ [OPI Plus temp1 temp1 1])

transExpr (PosOp e1 MinusMinus) tabl dest
  = undefined
--   = do temp1 <- newTemp 
--        code1 <- transExpr (VarAccess e1) tabl temp1 
--        reuseTemps 1
--        return (code1 ++ [OPI Minus temp1 temp1 1])

transExpr (FunCall id args) tabl dest
    = do  (code, temps) <- transArgs args tabl
          reuseTemps (length temps)
          return (code ++ [CALL dest id temps])

transExpr _ table dest 
  = error "not implemented"

-- translate functions arguments
-- each one gets a new temporary
transArgs :: [Exp] -> Table -> State Supply ([Instr], [Temp])
transArgs [] tabl = return ([], [])
transArgs (exp:exps) tabl
      = do temp <- newTemp 
           code <- transExpr exp tabl temp 
           (code', temps') <- transArgs exps tabl
           return (code++code', temp:temps')



-- translate a statement
transStm :: Stmt -> Table -> State Supply [Instr]

transStm (AssignStm (Id var) expr) tabl 
  = case Map.lookup var tabl of
      Nothing -> error "undefined variable"
      Just dest -> transExpr expr tabl dest

transStm (SimpleExp expr) tabl 
  = do dest <- newTemp
      --  dest <- newLabel
       transExpr expr tabl dest

transStm (IfThen cond stm1) tabl 
  = do ltrue  <- newLabel 
       lfalse <- newLabel 
       code0  <- transCond cond tabl ltrue lfalse 
       code1  <- transStm stm1 tabl
       return (code0 ++ [LABEL ltrue] ++
               code1 ++ [LABEL lfalse])

transStm (IfThenElse cond stm1 stm2) tabl
  = do ltrue <- newLabel 
       lfalse <- newLabel 
       lend <- newLabel 
       code0 <- transCond cond tabl ltrue lfalse 
       code1 <- transStm stm1 tabl 
       code2 <- transStm stm2 tabl 
       return (code0 ++ [LABEL ltrue] ++ code1 ++
               [JUMP lend, LABEL lfalse] ++ code2 ++
               [LABEL lend])

transStm  (While cond stm) tabl =
  do lcond <- newLabel
     lbody <- newLabel
     lend <- newLabel
     code1 <- transCond cond tabl lbody lend
     code2 <- transStm stm tabl 
     return ([LABEL lcond] ++ code1 ++
             [LABEL lbody] ++ code2 ++
             [JUMP lcond, LABEL lend])

transStm (Return expr) tabl =
  do dest <- newTemp
     code <- transExpr expr tabl  dest
     reuseTemps 1
     return (code ++ [RETURN dest])

transStm (Block2 decl stms) tabl
  = do temps <- newTemps (length decl)  --reserva tempórarios para variáveis locais
       let newTable = Map.union tabl (Map.fromList (zip (extractDeclIdentifiers decl) temps))
       result <- transStmList stms newTable
       reuseTemps(length decl)    -- reutiliza tempórarios das declarações
       return result

transStm (For assig cond incr stm) tabl 
 = undefined
  -- = do ltrue <- newLabel
  --      lloop <- newLabel
  --      lend <- newLabel
  --      (code0, newTable) <- transAssignFor assig tabl
  --      code1 <- transCond cond newTable ltrue lend
  --      dest <- newTemp
  --      code2 <- transExpr incr newTable dest
  --      code3 <- transStm stm newTable
  --      return (code0 ++ [LABEL lloop] ++
  --              code1 ++ [LABEL ltrue] ++ code3 ++ code2 ++
  --              [JUMP lloop, LABEL lend])



-- transAssignFor :: Stmt -> Table -> State Supply ([Instr], Table)
-- transAssignFor (AssignStm (Id var) expr) tabl 
--   = case Map.lookup var tabl of
--       Nothing -> error "undefined variable"
--       Just temp -> do temp <- newTemp
--                       code <- transExpr expr tabl temp
--                       return ((code),tabl)



-- translate a condition
transCond :: Exp -> Table -> Label -> Label -> State Supply [Instr]

transCond (Not e1) tabl ltrue lfalse 
  = do temp1 <- transCond e1 tabl lfalse ltrue
       return temp1

transCond (BoolLiteralTrue) _ ltrue _ = do return ([JUMP ltrue])

transCond (BoolLiteralFalse) _ _ lfalse = do return ([JUMP lfalse])

transCond (BinOp e1 rel e2) tabl ltrue lfalse 
  | rel == LessThan || rel == LessThanEqual || rel == Equal 
    || rel == NotEqual || rel == GreaterThan || rel == GreaterThanEqual = 
      do temp1 <- newTemp
         temp2 <- newTemp 
         code1 <- transExpr e1 tabl temp1
         code2 <- transExpr e2 tabl temp2
         reuseTemps 2
         return ( code1 ++ code2 ++
                  [COND temp1 rel temp2 ltrue lfalse] )
  --seguindo tradução slide 38 Aula 10
  | rel == And = 
        do l2 <- newLabel
           code1 <- transCond e1 tabl l2 lfalse
           code2 <- transCond e2 tabl ltrue lfalse
           return (code1 ++ [LABEL l2] ++ code2) 
  | rel == Or = 
      do l2 <- newLabel
         code1 <- transCond e1 tabl ltrue l2 
         code2 <- transCond e2 tabl ltrue lfalse
         return (code1 ++ [LABEL l2] ++ code2)
-- **** adicionar tratamento de numeros (intliteral)


-- translate a list of statements
-- translate individual statements and join the resulting instructions
transStmList :: [Stmt] -> Table -> State Supply [Instr]
transStmList [] tabl = return []
transStmList (stm:rest) tabl = do
  code1 <- transStm stm tabl 
  code2 <- transStmList rest tabl
  return (code1 ++ code2)



-- translate a function declaration 
transFunDef :: FunDef -> State Supply [Instr]
transFunDef  (FunDef fun args locals body) 
  = do targs <- newTemps (length args)      -- temporaries for arguments
       tlocals <- newTemps (length locals)  -- temporaries for locals
       -- setup symbol table
       let table = Map.fromList (zip args targs ++ zip locals tlocals)
       -- translate the body
       code <- transStmList body table
       -- return the code
       return (LABEL fun : code)



--função que nos argumentos da função , ex:  [Param TypeInt "ola",Param TypeInt "y"]
-- e retorna a lista de "nomes" , nest caso = ["ola","y"]
extractIdentifiers :: [Param] -> [Ident]
extractIdentifiers [] = []
extractIdentifiers (Param _ ident : rest) = ident : extractIdentifiers rest



-- retorna lista de identificadores de uma lista de declarações
-- ex: [[("x",TypeInt),("y",TypeInt)]] -> ["x","y"]
extractDeclIdentifiers :: [Decl] -> [Ident]
extractDeclIdentifiers decl = map (\x -> fst x) decl 



--Parece funcionar mas acho que tenho de corrigir definição de bloco para funcionar corretamente
transFunDecl :: FunDecl -> State Supply [Instr]
transFunDecl (FunDecl t fun args (Block2 decl stmt))
  = transFunDef (FunDef fun fun_args locals body)
  where 
    locals = extractDeclIdentifiers decl    -- idetificadores das declarações
    fun_args = extractIdentifiers args      -- identificadores dos argumentos das funções
    body = stmt                             -- lista de stmt



--tradução de multiplas funções
transFunDeclList :: Prog -> State Supply [Instr]
transFunDeclList (GDefn []) = return [] 
transFunDeclList (GDefn (fundecl:rest)) = do 
  code1 <- transFunDecl fundecl           --traduz uma declaração de função
  (temps,labels) <- State.get
  State.put (0,labels)                    -- Re-inicializa o numero de temps  
                                          --para próxima declaração de função
  code2 <- transFunDeclList (GDefn rest)  --traduz as restantes declarações de função
  return (code1 ++ code2) 



-- print an horizontal line
line :: IO () 
line = putStrLn (replicate 40 '-')



-- print a list of IR instructions
printIR :: [Instr] -> IO ()
printIR = mapM_ print

