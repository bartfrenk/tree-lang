{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module TreeLang.Lexer where

import           Control.Monad
import           Data.Functor.Identity
import           Text.Parsec
import qualified Text.Parsec.Token as T

type Parser s = Parsec s ()

type CharStream s = Stream s Identity Char

lexer :: CharStream s => T.GenTokenParser s () Identity
lexer = T.makeTokenParser style
  where
    ops = ["+", "*", "-", ";", "=="]
    names = ["if", "then", "else", "end", "elif"]
    style = T.LanguageDef
            { T.commentStart = ""
            , T.commentEnd = ""
            , T.commentLine = "#"
            , T.nestedComments = False
            , T.identStart = letter <|> char '_'
            , T.identLetter = alphaNum <|> char '_'
            , T.opStart = oneOf ":!#$%&*+./<=>?@^|-~"
            , T.opLetter = oneOf ":!#$%&*+./<=>?@^|-~"
            , T.reservedNames = names
            , T.reservedOpNames = ops
            , T.caseSensitive = True
            }

whitespace :: CharStream s => Parser s ()
whitespace = void $ many $ oneOf " \t"

lexeme :: CharStream s => Parser s a -> Parser s a
lexeme p = p <* whitespace

reserved :: CharStream s => String -> Parser s ()
reserved s = void $ lexeme $ string s

macro :: CharStream s => Parser s ()
macro = void $ char '$'

dot :: CharStream s => Parser s ()
dot = void $ char '.'

identifier :: CharStream s => Parser s String
identifier = T.identifier lexer

integer :: CharStream s => Parser s Integer
integer = lexeme (read <$> many1 digit)

op :: CharStream s => String -> Parser s ()
op s = void $ lexeme $ string s

quoted :: CharStream s => Parser s String
quoted = T.stringLiteral lexer

parens :: CharStream s => Parser s a -> Parser s a
parens = T.parens lexer

sep :: CharStream s => Parser s ()
sep = void $ skipMany1 $ lexeme $ oneOf ";\n"

linebreak :: CharStream s => Parser s ()
linebreak = void $ char '\n'

colon :: CharStream s => Parser s ()
colon = lexeme $ op ":" *> skipMany linebreak

float :: CharStream s => Parser s Double
float = T.float lexer
