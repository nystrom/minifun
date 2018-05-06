{
module Lexer(
  scanTokens,
  Token(..),
) where

import Data.Char(chr)
}

%wrapper "basic"

$digit       = [0-9]        -- digits
$alpha       = [a-zA-Z_]    -- alphabetic characters and underscore
$ln          = [\n\r]
$special     = [\\'ntrf]
$graphic     = $printable # $white
@escape      = \\ $special
@charlit     = \' (($graphic # $special) | @escape) \'


tokens :-
  "--"\-*.*                    ;
  $white                       ;
  fun                          {const TokenFun}
  let                          {const TokenLet}
  in                           {const TokenIn}
  if                           {const TokenIf}
  then                         {const TokenThen}
  else                         {const TokenElse}
  True                         {const TokenTrue}
  False                        {const TokenFalse}
  $digit+                      {\s -> TokenNum (read s)}
  \:=                          {const TokenAssign}
  \->                          {const TokenArrow}
  \=                           {const TokenBind}
  \+                           {const TokenAdd}
  \-                           {const TokenSub}
  \*                           {const TokenMul}
  \<                           {const TokenLt}
  \>                           {const TokenGt}
  \<=                          {const TokenLe}
  \>=                          {const TokenGe}
  \==                          {const TokenEqu}
  \/=                          {const TokenNeq}
  \&&                          {const TokenAnd}
  \|\|                         {const TokenOr}
  \!                           {const TokenBang}
  \(                           {const TokenLParen}
  \)                           {const TokenRParen}
  \,                           {const TokenComma }
  $alpha [$alpha $digit \_]*   {\s -> TokenVar s}
  




{
-- Each action has type :: String -> Token

-- The token type
data Token = TokenFun
           | TokenLet
           | TokenIn
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenComma
           | TokenLParen
           | TokenRParen
           | TokenTrue
           | TokenFalse
           | TokenVar String
           | TokenBind
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenLt
           | TokenGt
           | TokenLe
           | TokenGe
           | TokenEqu
           | TokenNeq
           | TokenAnd
           | TokenOr
           | TokenBang
           | TokenAssign
           | TokenArrow
           | TokenNum Int
           deriving (Show, Eq)

scanTokens = alexScanTokens

 
}


