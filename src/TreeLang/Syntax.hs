module TreeLang.Syntax where


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
  deriving (Eq, Show)


