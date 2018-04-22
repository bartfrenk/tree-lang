module Syntax where


type Name = String

data Expr
  = Case [(Expr, Expr)] Expr
  | Assign Name Expr
  | Seq Expr Expr
