module Syntax where

-- abstract syntax trees for the language
data Exp = Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Div Exp Exp
        | Lt Exp Exp | Le Exp Exp | Ge Exp Exp | Gt Exp Exp | Eq Exp Exp | Ne Exp Exp
        | And Exp Exp | Or Exp Exp
        | If Exp Exp Exp
        | Not Exp
        | Lit Value

        | Var String                -- x                is (Var "x")
        | Let String Exp Exp        -- let x = e1 in e2

        | Lambda String Exp         -- fun x -> e
        | Apply Exp Exp             -- e1 e2

        | New Exp                   -- new e
        | Deref Exp                 -- *e
        | Assign Exp Exp            -- x := 2+3

  deriving Show

data Value = IntVal Int | BoolVal Bool | ClosureVal String Exp Env | LocVal Int
  deriving Show

type Env = [(String, Value)]
