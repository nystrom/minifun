module Main where

import Control.Applicative ((<*))
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Parser (program, parseTokens)
import Syntax
import Eval

main = do
  input <- getLine
  case parseTokens input of
        Left error -> putStrLn error
        Right tokens -> do
            case program tokens of
               Left error -> putStrLn error
               Right e -> do
                  print e
                  let v = runIdentity (runReaderT (evalStateT (runExceptT (eval e)) []) [])
                  case v of
                    Left error -> putStrLn error
                    Right v -> print v


