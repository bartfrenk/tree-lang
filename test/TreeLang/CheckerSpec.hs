module TreeLang.CheckerSpec where


import           Test.Hspec
import           Text.Parsec     (ParseError)
import qualified Text.Parsec     as P

import           TreeLang.Lexer  (Parser)
import           TreeLang.Parser hiding (runParser)
import           TreeLang.Syntax


pparse :: Parser String a -> String -> Either ParseError a
pparse p str = P.runParser p () "" str



recordProgram :: String
recordProgram = unlines
  ["if $u.w == 1:",
   "    x = { bar = 1, foo = \"baz\" };",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]


