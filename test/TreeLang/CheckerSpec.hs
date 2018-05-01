module TreeLang.CheckerSpec where


import Data.Functor.Identity
import           Test.Hspec
import           Text.Parsec     (ParseError)
import qualified Text.Parsec     as P

import           TreeLang.Lexer  (Parser)
import           TreeLang.Parser hiding (runParser)
import           TreeLang.Syntax
import TreeLang.Checker
import TreeLang.Context


pparse :: Parser String a -> String -> Either ParseError a
pparse p str = P.runParser p () "" str

tyContext :: ContextT Ty Identity
tyContext = newContext [("u", pure $ newTyRecord [
                            ("w", TyInt),
                            ("z", TyString)]),
                        ("f", pure $ TyFunction
                          (newTyRecord [("x", TyInt)])
                          (newTyRecord [("y", TyInt)]))]

recordProgram :: String
recordProgram = unlines
  ["if $u == { w = 1, z = \"asd\" }:",
   "    x = { bar = 1, foo = \"baz\" };",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]

recordProgram2 :: String
recordProgram2 = unlines
  ["if $f(x=$u.z).y == 1:",
   "    x = { bar = 1, foo = \"baz\" };",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]





test s = do
  case pparse program s of
    Left e -> error (show e)
    Right ast -> checkProgram tyContext ast
