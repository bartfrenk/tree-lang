module TreeLang.Syntax where

import Data.List

type Name = String

type Program = [Statement]

type Op = String

data Statement
  = Assignment Name Expr
  | Cond [(Expr, Program)] (Maybe Program)
  deriving (Eq, Show)


data Expr
  = ContextMacro [String]
  | BinaryOp Op Expr Expr
  | IntLiteral Integer
  | StringLiteral String
  | FloatLiteral Double
  | BoolLiteral Bool
  deriving (Eq)

instance Show Expr where
  show (ContextMacro path) = "$" ++ intercalate "." path
  show (BinaryOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (IntLiteral i) = show i
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (FloatLiteral d) = show d
  show (BoolLiteral b) = show b

data OperatorType
  = Comparison
  | Equality

operatorType :: String -> Maybe OperatorType
operatorType op
  | op `elem` ["=="] = Just Equality
  | op `elem` ["<", ">", "<=", ">="] = Just Comparison
  | otherwise = Nothing

