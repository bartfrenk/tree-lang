module TreeLang.Examples where

import Control.Monad.Except
import Data.Functor.Identity


import TreeLang.Context
import TreeLang.Parser
import TreeLang.Checker
import TreeLang.Interpreter
import TreeLang.Syntax


parse :: Monad m => String -> ExceptT String m Program
parse str =
  case runParser program () "" str of
    Left e -> throwError (show e)
    Right r -> pure r

check :: Monad m => ContextT Ty m -> Program -> ExceptT String m Ty
check ctx pr = toError unTypeError $ checkProgram ctx pr

interpret :: Monad m => ContextT Expr m -> Program -> ExceptT String m Program
interpret ctx pr =
  toError unRuntimeError $ interpretProgram ctx pr

run :: Monad m => ContextT Ty m -> ContextT Expr m -> String -> ExceptT String m Program
run tyCtx valCtx s = do
  ast <- parse s
  _ <- check tyCtx ast
  interpret valCtx ast


test :: String -> ExceptT String Identity Program
test = run tyContext valContext

valContext :: Monad m => ContextT Expr m
valContext = newContext [("weather", valWeather)]

valWeather :: ContextObj Expr
valWeather = newContextObj
  [ ("temperature", FloatLiteral 0.0)
  , ("conditions", StringLiteral "rainy")
  ]

tyContext :: Monad m => ContextT Ty m
tyContext = newContext [("weather", tyWeather)]

tyWeather :: ContextObj Ty
tyWeather = newContextObj
  [ ("temperature", TyFloat)
  , ("conditions", TyString)
  ]

withTypeErrors :: String
withTypeErrors = unlines
  ["if $weather.temperature == 1:",
   "    x = 2",
   "    y = 3",
   "elif $weather.conditions == 2:",
   "    if $u.v == 3:",
   "        z = 4",
   "    end",
   "else:",
   "    z = 5",
   "end"
  ]


good2 :: String
good2 = unlines
  ["if $weather.temperature == 1.0:",
   "    x = 2",
   "    y = 3",
   "elif $weather.conditions == \"rainy\":",
   "    if $weather.temperature == 3.0:",
   "        z = 4",
   "    end",
   "else:",
   "    z = 5",
   "end"
  ]

good1 :: String
good1 = unlines
  ["if $weather.temperature > 1:",
   "    x = 2",
   "    y = 3",
   "elif $weather.conditions == \"rainy\":",
   "    if $weather.temperature == 3.0:",
   "        z = 4",
   "    end",
   "else:",
   "    z = 5",
   "end"
  ]


prs :: String
prs = unlines
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
  ["if $u.w == 1.22:",
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
   "    z = True",
   "end"
  ]

z :: String
z = unlines
  ["if $u.w == 1:",
   "    x = 2",
   "    y = 3",
   "elif $u.w == 2.000:",
   "    z = 4",
   "else:",
   "    z = 5",
   "end"
  ]