module Expressions where

data Expr = 
  EInt Int
  | EString String
  | EAdd Expr Expr
  | EConcat Expr Expr
  deriving (Show, Eq)