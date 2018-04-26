module TreeLang.Examples where

import Control.Monad.Except
import Haxl.Core

import TreeLang.Context
import TreeLang.Parser
import TreeLang.Checker
import TreeLang.Interpreter
import TreeLang.Syntax

import TreeLang.Haxl
import TreeLang.Haxl.Weather (tyWeather, valWeather)
import TreeLang.Haxl.Location (tyLocation, valLocation)
import TreeLang.Haxl.Location as L
import TreeLang.Haxl.Weather as W


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

test :: String -> ExceptT String Haxl Program
test = run tyContext valContext


haxl :: ExceptT String Haxl a -> IO (Either String a)
haxl act = do
  let stateStore = stateSet (W.UserState {}) $ stateSet (L.UserState {}) stateEmpty
  environ <- initEnv stateStore ()
  runHaxl environ (runExceptT act)

tyContext :: ContextT Ty Haxl
tyContext = newContext [ ("weather", tyWeather)
                       , ("location", tyLocation)
                       ]

valContext :: ContextT Expr Haxl
valContext = newContext [ ("weather", valWeather)
                        , ("location", valLocation)
                        ]

withTypeErrors :: String
withTypeErrors = unlines
  ["if $weather.temperature == 1.0:",
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

example :: IO String
example = readFile "res/example-2.tl"

-- Parsing this misses the second assignment
minimalParseFailure :: String
minimalParseFailure = unlines [
  "x = \"asdas\"",
  "y = 3"
  ]

-- while this is parsed successfully
counterExample1 :: String
counterExample1 = unlines [
  "x = \"asdas\";",
  "y = 3"
  ]

-- as is this
counterExample2 :: String
counterExample2 = unlines [
  "x = 2",
  "y = 3"
  ]

good2 :: String
good2 = unlines
  ["if $weather.conditions == \"asdas\":",
   "    x = $weather.temperature",
   "    y = 3",
   "end"
  ]

good1 :: String
good1 = unlines
  ["if $weather.temperature > 1:",
   "    x = 2",
   "    y = 3",
   "elif $weather.conditions == \"rainy\":",
   "    if $weather.temperature >= 0.0:",
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
