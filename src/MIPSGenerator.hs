module MIPSGenerator where

import IR
import Parser
import AST

import Data.Char


intermediateCodeToMips :: Instr -> [String]

intermediateCodeToMips (MOVE t1 t2)
   = ["\tmove $" ++ t1 ++ " , $" ++ t2]

intermediateCodeToMips (MOVEI t1 int) 
   = ["\tli $" ++ t1 ++ " , " ++ show int]

{-
This is shown in the following code fragment which multiplies the value in $t1 
by the value in $t2, and stores the result in $t0

mult $t1, $t2
mflo $t0  

This is shown in the following code fragment which divides the value in $t2 
by the value in $t3, and stores de non-fractional part to t1 and fractional part to t0

div $t2, $t3
mflo $t1   -->Guarda a parte inteira da divisão de t2 por t3
mfli $t0   -->Guarda a parte inteira da divisão de t2 por t3
-}

intermediateCodeToMips (OP binOp t1 t2 t3) = 
    case binOp of
        Plus -> ["\tadd $" ++ t1 ++ " , $" ++ t2 ++ " , $" ++ t3]
        Minus -> ["\tsub $" ++ t1 ++ " , $" ++ t2 ++ " , $" ++ t3]
        Mult -> ["\tmult $" ++ t2 ++ " , $" ++ t3, "\tmflo $" ++ t1]
        Div -> ["\tdiv $" ++ t2 ++ " , $" ++ t3, "\tmflo $" ++ t1]
        Mod -> ["\tdiv $" ++ t2 ++ " , $" ++ t3, "\tmfhi $" ++ t1]
intermediateCodeToMips (OPI binOp t1 t2 int) = 
    case binOp of
        Plus -> ["\taddi $" ++ t1 ++ " , $" ++ t2 ++ " , $" ++ show int]
        Minus -> ["\tsub $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ show int]
        Mult -> ["\tmult $" ++ t2 ++ " , $" ++ show int , "\tmflo $" ++ t1]
        Div -> ["\tdiv $" ++ t2 ++ " , " ++ show int, "\tmflo $" ++ t1]
        Mod -> ["\tdiv $" ++ t2 ++ " , " ++ show int, "\tmfhi $" ++ t1]
intermediateCodeToMips (LABEL l1) 
   | (l1 /= "\nscan_int" && l1 /= "\nprint_int" &&
      l1 /= "\nprint_str" && l1 /= "\n_return_" && 
      l1 /= "\n_startprogram_" && l1 /= "\n_stopprogram_" && 
      islabel == False) == True =     
        [l1 ++ ": "] ++
            ["\tsw $fp, -4($sp)  #save old $fp"] ++      --Video da Aula 13 (código a adcionar ao inicio de cada função)
            ["\tsw $ra, -8($sp)  #save return address"]  ++ 
            ["\tla $fp , 0($sp)  #setup frame pointer"] ++ 
            ["\tla $sp, -10($sp) #stack space for all 10 temps"]
   | otherwise = [l1 ++ ": "]   --funções pré-definidas (print_int ,scan_int ,print_str , _return_ e _startprogram_) .Também inclui Labels
    where 
        labels = map (\x -> "L" ++ show(x)) (take 100 [0..])    --Supondo que só temos lables de L0 até L99
        islabel = any (==l1) labels     
intermediateCodeToMips (JUMP l1) 
   = ["\tj " ++ l1]
intermediateCodeToMips (COND t1 relOp t2 l1 l2) =
    case relOp of
        Equal -> ["\tbne $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
        NotEqual -> ["\tbeq $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
        LessThan -> ["\tblt $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ l1, "\tj " ++ l2]
        GreaterThan -> ["\tbgt $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ l1, "\tj " ++ l2] 
        LessThanEqual -> ["\tbgt $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
        GreaterThanEqual -> ["\tblt $" ++ t1 ++ " , $" ++ t2 ++ " , " ++ l2, "\tj " ++ l1]
intermediateCodeToMips (CALL t funcName args)       -- t : = CALL F(r1,...,rn)
   = functionCallCode funcName args 0 ++ intermediateCodeToMips (MOVE t "v0")   -- Move t, $v0 #save result

intermediateCodeToMips (RETURN t)                   --RETURN t
    = ["\tmove $v0, $" ++ t  ++ "    #store result"] ++ 
      ["\tjal _return_     #jump and link to return"]



functionCallCode :: Label -> [Temp] -> Int -> [String]
functionCallCode funcName [] offset =
        [ "\tla $sp, " ++ show offset ++ "($sp)  #grow stack", 
          "\tjal " ++ funcName ++ "          #jump and link", 
          "\tla $sp, " ++ show (offset * (-1)) ++ "($sp)   #shrink stack"]
    
functionCallCode funcName (a : args) oldOffset =
  let newOffset = oldOffset - 4
      storeArg1 = ["\tsw " ++ "$" ++ a ++ " , " ++ show newOffset ++ "($sp) #store function arguments"]
      restArgs = functionCallCode funcName args newOffset
   in storeArg1 ++ restArgs



iOFunctions :: String -> [String]
iOFunctions "scan_int" =
    intermediateCodeToMips (LABEL "\nscan_int") ++
    ["\tli $v0, 5", "\tsyscall", "\tjr $ra"]

iOFunctions "print_int" =
    intermediateCodeToMips (LABEL "\nprint_int") ++
    ["\tli $v0, 1", "\tlw $a0, 0($sp)", "\tsyscall", "\tjr $ra"]

iOFunctions "print_str" =
    intermediateCodeToMips (LABEL "\nprint_str") ++
    ["\tli $v0, 4", "\tlw $a0, 0($sp)", "\tsyscall", "\tjr $ra"]



returnFunction :: [String]
returnFunction = intermediateCodeToMips(LABEL "\n_return_")++
    ["\tla $sp, 0($fp)  #restore stack pointer"] ++ 
    ["\tlw $ra, -8($sp) #restore return address"] ++ 
    ["\tlw $fp, -4($sp) #restore frame pointer"] ++ 
    ["\tjr $ra          #return"]



startProgram :: [String]
startProgram = intermediateCodeToMips(LABEL "\n_startprogram_") ++
    ["\tjal main         #start program"]



stopProgram :: [String]
stopProgram = intermediateCodeToMips (LABEL "\n_stopprogram_") ++
    ["\tli $v0, 10"] ++ 
    ["\tsyscall          #Exit the program"]



mipsCodeRecursive :: [Instr] -> [String]
mipsCodeRecursive [] = []
mipsCodeRecursive (x:xs) = (intermediateCodeToMips x) ++ (mipsCodeRecursive xs)



finalMIPSCode :: [Instr] -> [String]
finalMIPSCode instr = [".text"] 
                    ++ startProgram
                    ++ stopProgram
                    ++ mipsCodeRecursive instr 
                    ++ iOFunctions "scan_int" 
                    ++ iOFunctions "print_int" 
                    ++ iOFunctions "print_str"
                    ++ returnFunction
