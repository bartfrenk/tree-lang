module TreeLang.InterpreterSpec where

import Data.Functor.Identity
import           Test.Hspec
import           Text.Parsec     (ParseError)
import qualified Text.Parsec     as P

import           TreeLang.Lexer  (Parser)
import           TreeLang.Parser hiding (runParser)
import           TreeLang.Syntax
import TreeLang.Checker
import TreeLang.Context
import TreeLang.Interpreter


pparse :: Parser String a -> String -> Either ParseError a
pparse p str = P.runParser p () "" str

recordProgram :: String
recordProgram = unlines
  ["if $u == { w = 1, z = \"asd\" }:",
   "    x = { bar = 1, foo = \"baz\" };",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]

ctx :: ExprContextT Identity
ctx = newExprContext [("u", \_ -> pure $ newRecord [
                        ("w", IntLiteral 1),
                        ("z", StringLiteral "asd")])]

test s = do
  case pparse program s of
    Left e -> error (show e)
    Right ast -> interpretProgram ctx ast

