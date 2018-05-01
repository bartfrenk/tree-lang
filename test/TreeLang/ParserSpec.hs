module TreeLang.ParserSpec where

import           Test.Hspec
import           Text.Parsec     (ParseError)
import qualified Text.Parsec     as P

import           TreeLang.Lexer  (Parser)
import           TreeLang.Parser hiding (runParser)
import           TreeLang.Syntax

pparse :: Parser String a -> String -> Either ParseError a
pparse p str = P.runParser p () "" str


txt_1 :: String
txt_1 = unlines
  ["if $u.w == 1:",
   "    z = 1",
   "end",
   "if $u.w == 1:",
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

-- ast_1 :: Program
-- ast_1 =
--   [ Cond
--       [ ( BinaryOp "==" (ContextMacro ["u", "w"]) (IntLiteral 1)
--         , [Assignment "z" (IntLiteral 1)])
--       ]
--       Nothing
--   , Cond
--       [ ( BinaryOp "==" (ContextMacro ["u", "w"]) (IntLiteral 1)
--         , [Assignment "x" (IntLiteral 2), Assignment "y" (IntLiteral 3)])
--       , ( BinaryOp "==" (ContextMacro ["u", "w"]) (IntLiteral 2)
--         , [ Cond
--               [ ( BinaryOp "==" (ContextMacro ["u", "v"]) (IntLiteral 3)
--                 , [Assignment "z" (IntLiteral 4)])
--               ]
--               Nothing
--           ])
--       ]
--       (Just [Assignment "z" (IntLiteral 5)])
--   ]

-- spec :: Spec
-- spec = do

--   describe "contextMacro parser" $ do
--     it "should parse a macro without accessors" $
--       (runParser contextMacro "$name") == (Right $ ContextMacro ["name"])
--     it "should parse a macro with accessors" $
--       (runParser contextMacro "$x.y") == (Right $ ContextMacro ["x", "y"])

--   describe "expression parser" $ do
--     it "should parse == as a binary operator" $
--       (runParser expr "1 == 2") ==
--       (Right $ BinaryOp "==" (IntLiteral 1) (IntLiteral 2))

--   describe "program parser" $ do
--     it "should correctly parse txt_1 as ast_1" $
--       (runParser program txt_1) == Right ast_1


recordProgram :: String
recordProgram = unlines
  ["if $u.w == 1:",
   "    x = { bar = 1, foo = \"baz\" };",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]

recordProgram2 :: String
recordProgram2 = unlines
  ["if $u(s=1, t=$bla.x).w == 1:",
   "    x = {};",
   "    y = 3",
   "else:",
   "    z = 5",
   "end"
  ]



fieldProgram :: String
fieldProgram = unlines
  ["$weather.temperature > 1.0"]

fieldProgram2 :: String
fieldProgram2 = unlines
  ["$u > w > z > u > 1.0"]
