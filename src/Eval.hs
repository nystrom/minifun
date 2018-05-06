module Eval where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Syntax

toNumber :: Value -> EvalM Int
toNumber (IntVal v) = return v
toNumber (BoolVal v) = throwError "type error: cannot convert boolean to int"
toNumber (ClosureVal x e env) = throwError "type error: cannot convert function to int"
toNumber (LocVal v) = throwError "type error: cannot convert location to int"

toBool :: Value -> EvalM Bool
toBool (BoolVal v) = return v
toBool (IntVal v) = throwError "type error: cannot convert int to boolean"
toBool (ClosureVal x e env) = throwError "type error: cannot convert function to boolean"
toBool (LocVal v) = throwError "type error: cannot convert location to boolean"

binop :: (a -> a -> b) -> (Value -> EvalM a) -> (b -> Value) -> Exp -> Exp -> EvalM Value
binop op convert construct e1 e2 = do
  u1 <- eval e1
  u2 <- eval e2
  v1 <- convert u1
  v2 <- convert u2
  return $ construct (op v1 v2)

arith :: (Int -> Int -> Int) -> Exp -> Exp -> EvalM Value
arith op = binop op toNumber IntVal 

relation :: (Int -> Int -> Bool) -> Exp -> Exp -> EvalM Value
relation op = binop op toNumber BoolVal

boolean :: (Bool -> Bool -> Bool) -> Exp -> Exp -> EvalM Value
boolean op = binop op toBool BoolVal

type Store = [(Int, Value)]

type EvalM a = ExceptT String (StateT Store (ReaderT Env Identity)) a

eval :: Exp -> EvalM Value

eval (Lambda x e) = do
        env <- ask
        return $ ClosureVal x e env

eval (Apply e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case v1 of
                ClosureVal x e env -> local (\_ -> (x,v2):env) $ eval e
                _ -> throwError "cannot apply a non-function value"

eval (If e0 e1 e2) = do
        v0 <- eval e0
        case v0 of
                BoolVal True -> eval e1
                BoolVal False -> eval e2
                _ -> throwError "if condition must be a boolean"

eval (Assign e1 e2) = do
        v1 <- eval e1
        v2 <- eval e2
        case v1 of
          LocVal n ->
            do
              modify ((n,v2):)
              return v2
          _ -> throwError $ "cannot assign to non-location"

eval (New e) = do
        v <- eval e
        h <- get
        let fresh = 1 + maximum (map fst h)
        modify ((fresh, v):)
        return v

eval (Deref e) = do
        v <- eval e
        case v of
          LocVal n -> do
                        h <- get
                        case lookup n h of
                                Just v -> return v
                                Nothing -> throwError $ "location not found"
          _ -> throwError $ "cannot dereference a non-location"

eval (Var x) = do
        env <- ask
        case lookup x env of
                Just v -> return v
                Nothing -> throwError $ "no variable " ++ x ++ " in scope"

eval (Lit v) = return v
eval (Add e1 e2) = arith (+) e1 e2
eval (Sub e1 e2) = arith (-) e1 e2
eval (Mul e1 e2) = arith (*) e1 e2
eval (Div e1 e2) = arith div e1 e2
eval (And e1 e2) = boolean (&&) e1 e2
eval (Or e1 e2) = boolean (||) e1 e2
eval (Lt e1 e2) = relation (<) e1 e2
eval (Le e1 e2) = relation (<=) e1 e2
eval (Gt e1 e2) = relation (>) e1 e2
eval (Ge e1 e2) = relation (>=) e1 e2
eval (Eq e1 e2) = do
        u1 <- eval e1
        u2 <- eval e2
        case (u1, u2) of
          (IntVal v1, IntVal v2) -> return $ BoolVal (v1 == v2)
          (BoolVal v1, BoolVal v2) -> return $ BoolVal (v1 == v2)
          (LocVal v1, LocVal v2) -> return $ BoolVal (v1 == v2)
          _ -> throwError "type error: cannot compare closures or values of different types"
eval (Ne e1 e2) = do
        u1 <- eval e1
        u2 <- eval e2
        case (u1, u2) of
          (IntVal v1, IntVal v2) -> return $ BoolVal (v1 /= v2)
          (BoolVal v1, BoolVal v2) -> return $ BoolVal (v1 /= v2)
          (LocVal v1, LocVal v2) -> return $ BoolVal (v1 /= v2)
          _ -> throwError "type error: cannot compare closures or values of different types"

