{
module Lexer where 
}
%wrapper "basic"

$alpha = [_a-zA-Z]
$digit = [0-9]
$white = [\ \r\t\n\v\b\f\a] -- caracteres no c0 Reference: <esc>


tokens :-
$white+                    ; --ignora caracteres em branco
if                         { \_ -> IF }
else                       { \_ -> ELSE }
while                      { \_ -> WHILE }
for                        { \_ -> FOR }

int                        { \_ -> INT }
bool                       { \_ -> BOOL }
string                     { \_ -> STRING }

true                       { \_ -> TRUE }
false                      { \_ -> FALSE }

return                     { \_ -> RETURN }
break                      { \_ -> BREAK }
continue                   { \_ -> CONTINUE }

\(                         { \_ -> LPAREN }
\)                         { \_ -> RPAREN }
\{                         { \_ -> LBRACE }
\}                         { \_ -> RBRACE }
\,                         { \_ -> COMMA }
\;                         { \_ -> SEMICOLON }
\[                         { \_ -> LBRACKET } -- Usa nos arrays ou o programa é simples demais? No exemplo do enunciado usa.
\]                         { \_ -> RBRACKET }

-- Pos Operators
"++"      { \_ -> PLUSPLUS } -- !! Perguntar ao professor qual o nome deste token
"--"      { \_ -> MINUSMINUS }

-- Operadores aritmeticos: +, -, *, /, %
"+"       { \_ -> PLUS }
"-"       { \_ -> MINUS }
"*"       { \_ -> MULT }
"/"       { \_ -> DIV }
"%"       { \_ -> MOD}

-- Operadores relacionais: ==, !=, <, <=, >, >=
"=="      { \_ -> EQUAL }
"!="      { \_ -> NOTEQUAL }
"<"       { \_ -> LTHAN }
"<="      { \_ -> LTHANEQUAL }
">"       { \_ -> GTHAN }
">="      { \_ -> GTHANEQUAL }

-- Operadores lógicos !, && e || (com avaliação short-circuit)
"!"       { \_ -> NOT }
"&&"      { \_ -> AND }
"||"      { \_ -> OR }

-- Operador de atribuição simples
"="       { \_ -> ASSIGN}

-- Funcoes de entrada e saida
-- "print_int"  {\_ -> PRINTINT}
-- "scan_int"   {\_ -> SCANINT}
-- "print_str"  {\_ -> PRINTSTR}


$alpha($alpha|$digit)*     { \s -> ID s }
-- $alpha($alpha|$digit)*=    { \s -> VAR s }


$digit+                    { \s -> NUM (read s) }  -- c0 reference <decnum>: 0 | [1-9][0-9]*
\/\/.*                     ; -- ignora comentarios em uma linha
--"/*"(\n|.)*"*/"            ;ignora comentarios multilinha
"/*" ([^\*]|[\r\n] |(\*+ ([^\*\/] | [\r\n])) )*\*+"/"                    ;--comentario multilinha(https://blog.ostermiller.org/finding-comments-in-source-code-using-regular-expressions/)
\"[^\0\"]*\"               { \s -> STR s }  -- é pra ser qualquer caractere que não seja (\0) ou (")

{


data Token =  IF
            | ELSE
            | WHILE
            | FOR
            | INT
            | BOOL
            | STRING
            | TRUE
            | FALSE
            | RETURN
            | BREAK
            | CONTINUE
            | LPAREN
            | RPAREN
            | LBRACE
            | RBRACE
            | COMMA
            | SEMICOLON
            | ID String 
            | NUM Int
            | PLUSPLUS
            | MINUSMINUS
            | PLUS
            | MINUS
            | MULT
            | DIV
            | MOD
            | EQUAL
            | NOTEQUAL
            | LTHAN
            | LTHANEQUAL
            | GTHAN
            | GTHANEQUAL
            | NOT
            | AND
            | OR
            | LBRACKET
            | RBRACKET
            | ASSIGN
            | PRINTINT
            | SCANINT
            | PRINTSTR
            | STR String
            deriving (Eq, Show)

}
