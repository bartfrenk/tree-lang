{-# LANGUAGE FlexibleContexts #-}
module TreeLang.Parser where

import Text.Show.Pretty

import Text.Parsec
import           Data.Functor.Identity
import Text.Parsec.Expr

import TreeLang.Syntax
import TreeLang.Lexer


contextMacro :: CharStream s => Parser s Expr
contextMacro = ContextMacro <$> (macro *> sepBy1 identifier dot)

intLiteral :: CharStream s => Parser s Expr
intLiteral = IntLiteral <$> integer

stringLiteral :: CharStream s => Parser s Expr
stringLiteral = StringLiteral <$> quoted

term :: CharStream s => Parser s Expr
term = parens expr
   <|> contextMacro
   <|> intLiteral
   <|> stringLiteral
   <?> "term"

table :: CharStream s => OperatorTable s () Identity Expr
table = [[Infix (op "==" >> (pure $ BinaryOp "==")) AssocNone]]

expr :: CharStream s => Parser s Expr
expr = buildExpressionParser table term <?> "expression"

assignment :: CharStream s => Parser s Statement
assignment = do
  name <- identifier
  op "="
  ex <- expr
  pure $ Assignment name ex

statement :: CharStream s => Parser s Statement
statement = cond <|> assignment <?> "statement"

cond :: CharStream s => Parser s Statement
cond = do
  if_ <- guardClause "if"
  elif_ <- many $ guardClause "elif"
  else_ <- optionMaybe $ elseClause
  reserved "end"
  pure $ Cond (if_ : elif_) else_
  where
    guardClause keyword = do
      try $ reserved keyword
      guard <- expr
      colon
      pr <- program
      pure (guard, pr)
    elseClause = do
      try $ reserved "else"
      colon
      program

program :: CharStream s => Parser s Program
program = sepEndBy1 statement sep

s :: String
s = unlines
  ["1 == 2; 3 == $hello",
   "5"]

t :: String
t = unlines
  ["x = 1; y = 2",
   "z = ($u.w == 1)"]

u :: String
u = unlines
  ["if $u.w == 1:",
   "    x = 2",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]


w :: String
w = unlines
  ["if $u.w == 1:",
   "    x = 2",
   "    y = 3",
   "elif $u.w == 2:",
   "    if $u.v == 3:",
   "        z = 4",
   "    end",
   "else:",
   "    z = 5",
   "end"
  ]


a :: String
a = unlines
  ["if $u.w == 1:",
   "    z = 1",
   "end"
  ]

z :: String
z = unlines
  ["if $u.w == 1:",
   "    x = 2",
   "    y = 3",
   "elif $u.w == 2:",
   "    z = 4",
   "else:",
   "    z = 5",
   "end"
  ]


pparse :: Show a => Parser String a -> String -> IO ()
pparse p str =
  case runParser p () "" str of
    Left e -> print e
    Right r -> putStrLn $ ppShow r
