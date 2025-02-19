module Main where

import Parser
import Lexer
import AST
import IR
import CodeGen
import           Data.Map (Map)
import qualified Data.Map as Map


import MIPSGenerator
import Control.Exception
import Control.Monad.State 



--exemplos expressões
example1 = BinOp (BinOp (IntLiteral 2) Plus (IntLiteral 3)) Plus (IntLiteral 1)
example2 = BoolLiteralFalse
example3 = Str "Isto e uma String"
example4 = FunCall "foo" [IntLiteral 234,BoolLiteralFalse]
example5
  = BinOp 
    (FunCall "f" [BinOp (IntLiteral 2) Minus  (IntLiteral 2)])
    Plus
    (FunCall "g" [BinOp (IntLiteral 3) Mult  (IntLiteral 4)])
example7 = VarAccess (Id "x")

--exemplos statements
example8
  = IfThen
    (BinOp (VarAccess (Id"x")) LessThan (IntLiteral 0))
    (AssignStm (Id "y") (IntLiteral 1))

example9
  = IfThenElse
    (BinOp (VarAccess( Id "x")) LessThan (IntLiteral 0))
    (AssignStm (Id "y") (IntLiteral 1))
    (AssignStm (Id "y") (IntLiteral 2))
example10
  = While (BinOp  (IntLiteral 0) LessThan (VarAccess (Id "b")))
    ( Block2 [("x",TypeInt),("y",TypeInt)] 
            [ AssignStm (Id "r") (BinOp  (VarAccess (Id "a")) Mod (VarAccess (Id "b")))
            , AssignStm (Id "a") (VarAccess (Id "b"))
            , AssignStm (Id "b") (VarAccess (Id "r"))
            ]
    )

example11 -- Not
  = IfThen
    (Not (BinOp (VarAccess (Id "x")) LessThan (IntLiteral 0)))
    (AssignStm (Id "y") (IntLiteral 1))

{-
example12 -- PosOp
  = IfThen
    (Not (BinOp (VarAccess (Id "x")) LessThan (IntLiteral 0)))
   (SimpleExp (PosOp (Id "y") PlusPlus) )
-}

example13 = FunDecl TypeInt "max3" 
            [Param TypeInt "x",Param TypeInt "y",Param TypeInt "z"] 
            (Block2 
            [
              ("m",TypeInt)
            ] 
            [
              AssignStm (Id "m") (VarAccess (Id "x")),
              IfThen (BinOp (VarAccess (Id "m")) LessThan (VarAccess (Id "y"))) 
              (AssignStm (Id "m") (VarAccess (Id "y"))),
              IfThen (BinOp (VarAccess (Id "m")) LessThan (VarAccess (Id "z"))) 
              (AssignStm (Id "m") (VarAccess (Id "z"))),
              Return (VarAccess (Id "m"))
            ])

example14 = FunDecl TypeInt "max" 
            [Param TypeInt "x",Param TypeInt "y"] 
            (Block2
             [] 
             [IfThenElse (BinOp (VarAccess (Id "x")) LessThan (VarAccess (Id "y"))) 
             (Return (VarAccess (Id "y"))) 
             (Return (VarAccess (Id "x")))]
            )

example15 = FunDecl TypeInt "max3" 
            [Param TypeInt "x",Param TypeInt "y",Param TypeInt "z"] 
            (Block2 
            [] 
            [Return (FunCall "max" [VarAccess (Id "x"),
            FunCall "max" [VarAccess (Id "y"),
            VarAccess (Id "z")]])]
            )          

example16 = GDefn 
            [FunDecl TypeInt "max3" 
            [Param TypeInt "x",Param TypeInt "y",Param TypeInt "z"]
             (Block2 
             [] 
             [Return (FunCall "max" [VarAccess (Id "x"),FunCall "max" [VarAccess (Id "y"),VarAccess (Id "z")]])]
             )
            ] 
            
example17 = GDefn 
            [FunDecl TypeInt "max3" 
            [Param TypeInt "x",Param TypeInt "y",Param TypeInt "z"] 
            (Block2 
            [("m",TypeInt)] 
            [AssignStm (Id "m") (VarAccess (Id "x")),
            IfThen (BinOp (VarAccess (Id "m")) LessThan (VarAccess (Id "y"))) 
            (AssignStm (Id "m") (VarAccess (Id "y"))),
            IfThen (BinOp (VarAccess (Id "m")) LessThan (VarAccess (Id "z"))) 
            (AssignStm (Id "m") (VarAccess (Id "z"))),
            Return (VarAccess (Id "m"))
            ]
            ),
            FunDecl TypeInt "max3Recursive" 
            [Param TypeInt "x",Param TypeInt "y",Param TypeInt "z"] 
            (Block2 
            [] 
            [Return (FunCall "max" [VarAccess (Id "x"),
            FunCall "max" [VarAccess (Id "y"),
            VarAccess (Id "z")]])
            ]
            )
            ]  

main :: IO ()
main = do

        txt <- getContents
        
        line
        putStrLn "*** Lexer ****\n"

        let aTokens = alexScanTokens txt
        print (aTokens)

        line
        putStrLn "*** Parser ****\n"

        let pTokens = parser aTokens
        print (pTokens)

        line
        putStrLn "*** Intermediate Code ****\n"

        let intermediateCodeSupply = transFunDeclList (pTokens)
        let codeGenerated = runCodeGen (intermediateCodeSupply)
        printIR (codeGenerated)

        line
        putStrLn "*** MIPS Code ****\n"

        let intermed = evalState (transFunDeclList pTokens) (0, 0)
        
        let mipsCode = unlines (finalMIPSCode intermed)

        putStrLn (mipsCode)

        line



    {-
     line
     print example4
     printIR (runCodeGen (do tx <- newTemp
                             dest <- newTemp
                             let tabl = Map.fromList [("x", tx)]
                             transExpr example4 tabl dest ))
    -}
    --  line 
    --  print example9
    --  printIR (runCodeGen (do tx <- newTemp
    --                          ty <- newTemp
    --                          let tabl = Map.fromList [("x", tx), ("y", ty)]
    --                          transStm example9 tabl ))
    -- Example 10
    --  line 
    --  print example10
    --  printIR (runCodeGen (do ta <- newTemp
                            --  tb <- newTemp
                            --  tr <- newTemp
                            --  let tabl = Map.fromList [("a", ta), ("b", tb), ("r",tr)]
                            --  transStm example10 tabl )) 
     
    -- Example 8
    --  line  
    --  print example8
    --  printIR (runCodeGen (do tx <- newTemp
    --                          ty <- newTemp
    --                          let tabl = Map.fromList [("x", tx), ("y", ty)]
    --                          transStm example8 tabl ))
    -- Example 9 

    -- Example 11
    --  line  
    --  print example11
    --  printIR (runCodeGen (do tx <- newTemp
    --                          ty <- newTemp
    --                          let tabl = Map.fromList [("x", tx), ("y", ty)]
    --                          transStm example11 tabl ))
    --  line 
    --  print example13
    --  printIR (runCodeGen (transFunDecl example13))

    --  line
    --  print example14
    --  printIR (runCodeGen (transFunDecl example14)) 

    --  line
    --  print example15
    --  printIR (runCodeGen (transFunDecl example15)) 
     
    --  line 
    --  print example16
    --  printIR (runCodeGen (transFunDeclList example16)) 
    --  line 
    --  print example17
    --  printIR (runCodeGen (transFunDeclList example17))

-- Este programa lê toda a entrada padrão e verifica se respeita a gramática.
-- se sim: imprime ();
-- caso contrário: lança uma exceção.


