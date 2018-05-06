{

module Parser(
  program,
  parseTokens
) where

import Lexer
import Syntax

}

%monad {Either String} { (>>=) } {return}

%name program Program
%tokentype { Token }
%error     { parseError }

%token
  fun                          { TokenFun }
  let                          { TokenLet }
  in                           { TokenIn }
  if                           { TokenIf }
  then                         { TokenThen }
  else                         { TokenElse }
  true                         { TokenTrue }
  false                        { TokenFalse }
  NUM                          { TokenNum $$ }
  '='                          { TokenBind }
  '+'                          { TokenAdd }
  '-'                          { TokenSub }
  '*'                          { TokenMul }
  '<'                          { TokenLt }
  '>'                          { TokenGt }
  '<='                         { TokenLe }
  '>='                         { TokenGe }
  '=='                         { TokenEqu }
  '/='                         { TokenNeq }
  '&&'                         { TokenAnd }
  '||'                         { TokenOr }
  '!'                          { TokenBang }
  ':='                         { TokenAssign }
  '->'                         { TokenArrow }
  '('                          { TokenLParen }
  ')'                          { TokenRParen }
  VAR                          { TokenVar $$ }
  


%nonassoc  let if fun
%right in then else '=' '->'
%right     ':='
%right     '||'
%right     '&&'
%nonassoc  '/=' '==' '<' '>' '>=' '<='
%left      '+' '-'
%left      '*'
%nonassoc  NEG '!'
%%

Program        : Expr                              { $1 }

Expr           : let VAR '=' Expr in Expr          { Let $2 $4 $6 }
               | fun VAR '->' Expr                 { Lambda $2 $4 }
               | if Expr then Expr else Expr       { If $2 $4 $6 }
               | Binary                            { $1 }

Binary         : Binary ':=' Binary                { Assign $1 $3 }
               | Binary '||' Binary                { Or $1 $3 }
               | Binary '&&' Binary                { And $1 $3 }
               | Binary '/=' Binary                { Ne $1 $3 }
               | Binary '==' Binary                { Eq $1 $3 }
               | Binary '<' Binary                 { Lt $1 $3 }
               | Binary '>' Binary                 { Gt $1 $3 }
               | Binary '>=' Binary                { Ge $1 $3 }
               | Binary '<=' Binary                { Le $1 $3 }
               | Binary '+' Binary                 { Add $1 $3 }
               | Binary '-' Binary                 { Sub $1 $3 }
               | Binary '*' Binary                 { Mul $1 $3 }
               | '-' Binary     %prec NEG          { Sub (Lit (IntVal 0)) $2 }
               | '!' Binary     %prec NEG          { Not $2 }
               | Term                              { $1 }

Term           : Term Factor                       { Apply $1 $2 }
               | Factor                            { $1 }

Factor         : '(' Expr ')'                      { $2 }
               | false                             { Lit (BoolVal False) }
               | true                              { Lit (BoolVal True) }
               | NUM                               { Lit (IntVal $1) }
               | VAR                               { Var $1 }


{

parseError (l:ls) = Left (show l)
parseError [] = Left "Unexpected end of input"

parseTokens :: String -> Either String [Token]
parseTokens s = Right $ scanTokens s

}
