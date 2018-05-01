{-# LANGUAGE FlexibleContexts #-}
module TreeLang.Parser (module TreeLang.Parser,
                        runParser) where


import Text.Parsec
import Data.Functor.Identity
import Text.Parsec.Expr
import qualified Data.Map.Strict as Map

import TreeLang.Syntax
import TreeLang.Lexer


contextMacro :: CharStream s => Parser s Expr
contextMacro = ContextMacro <$> (macro *> identifier)

intLiteral :: CharStream s => Parser s Expr
intLiteral = IntLiteral <$> integer

stringLiteral :: CharStream s => Parser s Expr
stringLiteral = StringLiteral <$> quoted

floatLiteral :: CharStream s => Parser s Expr
floatLiteral = FloatLiteral <$> float

boolLiteral :: CharStream s => Parser s Expr
boolLiteral = BoolLiteral <$> bool

term :: CharStream s => Parser s Expr
term = parens expr
   <|> contextMacro
   <|> try floatLiteral
   <|> try intLiteral
   <|> try stringLiteral
   <|> try boolLiteral
   <|> try record
   <|> field
   <?> "term"

table :: CharStream s => OperatorTable s () Identity Expr
table = [[ Infix (char '.' >> (pure $ BinaryOp ".")) AssocLeft
         , Infix (op "==" >> (pure $ BinaryOp "==")) AssocLeft
         , Infix (op "<" >> (pure $ BinaryOp "<")) AssocLeft
         , Infix (op ">" >> (pure $ BinaryOp ">")) AssocLeft
         , Infix (op "<=" >> (pure $ BinaryOp "<=")) AssocLeft
         , Infix (op ">=" >> (pure $ BinaryOp ">=")) AssocLeft
         ]]

expr :: CharStream s => Parser s Expr
expr = buildExpressionParser table term <?> "expression"

field :: CharStream s => Parser s Expr
field = Field <$> identifier

assignment :: CharStream s => Parser s Statement
assignment = do
  name <- identifier
  op "="
  ex <- expr
  pure $ Assignment name ex

statement :: CharStream s => Parser s Statement
statement = cond <|> assignment <?> "statement"

record :: CharStream s => Parser s Expr
record = Record . Map.fromList <$> braces (sepBy1 field (sep <|> comma))
  where field = do
          name <- identifier
          _ <- lexeme $ string "="
          val <- expr
          pure (name, val)

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
