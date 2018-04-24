{-# LANGUAGE FlexibleContexts #-}
module TreeLang.Checker where

import Data.Functor.Identity
import Control.Monad.Except
import Data.Map (Map, (!?))
import Control.Monad.Catch
import qualified Data.Map as Map
import Data.Typeable
import Data.List (intercalate)

import TreeLang.Syntax

import qualified TreeLang.Parser as P

pprogram :: Monad m => String -> ExceptT TypeError m Program
pprogram pr = case P.runParser P.program () "" pr of
  Left err -> throwError $ ParseError err
  Right ast -> pure ast

test :: Monad m => TypeContextT m -> String -> ExceptT TypeError m Ty
test ctx pr = pprogram pr >>= checkProgram ctx


data Ty
  = TyInt
  | TyString
  | TyBool
  | TyUnit
  deriving (Eq, Show)

data TyContextMacro
  = Atom Ty | Object (Map String TyContextMacro)

tyWeather :: TyContextMacro
tyWeather = Object $ Map.fromList
  [ ("temperature", Atom TyInt)
  , ("conditions", Atom TyString)
  ]

bad :: String
bad = unlines
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


good :: String
good = unlines
  ["if $weather.temperature == 1:",
   "    x = 2",
   "    y = 3",
   "elif $weather.conditions == \"rainy\":",
   "    if $weather.temperature == 3:",
   "        z = 4",
   "    end",
   "else:",
   "    z = 5",
   "end"
  ]



tyContext :: TypeContext
tyContext = TypeContextT (Map.fromList [("weather", pure tyWeather)])

data TypeContextT m
  = TypeContextT (Map String (m TyContextMacro))


lookupTyContextMacro :: Monad m => TypeContextT m -> String -> ExceptT TypeError m TyContextMacro
lookupTyContextMacro (TypeContextT m) name =
  case m !? name of
    Nothing -> throwError $ TypeError $ "could not find context macro object " ++ name
    Just mTyContextMacro -> ExceptT $ Right <$> mTyContextMacro


lookupTy :: Monad m => TyContextMacro -> [String] -> ExceptT TypeError m Ty
lookupTy tyContextMacro path = lookupTy' tyContextMacro path
  where
    lookupTy' (Atom ty) [] = pure ty
    lookupTy' (Object _) [] =
      throwError $ TypeError $ "no type for " ++ intercalate "." path
    lookupTy' (Atom _) _ =
      throwError $ TypeError $ "no type for " ++ intercalate "." path
    lookupTy' (Object m) (x:xs) =
      case m !? x of
        Nothing ->
          throwError $ TypeError $ "no type for " ++ intercalate "." path
        Just tyContextMacro' ->
          lookupTy' tyContextMacro' xs

data TypeError
  = TypeError String
  | ParseError P.ParseError
  deriving (Show, Typeable)

instance Exception TypeError



type TypeContext = TypeContextT Identity

emptyTypeContext :: TypeContext
emptyTypeContext = TypeContextT Map.empty

checkProgram :: Monad m => TypeContextT m -> Program -> ExceptT TypeError m Ty
checkProgram ctx program = last <$> forM program (checkStatement ctx)

checkStatement :: Monad m => TypeContextT m -> Statement -> ExceptT TypeError m Ty
checkStatement ctx (Assignment _ expr) = checkExpr ctx expr >> pure TyUnit
checkStatement ctx (Cond guardedClauses mElseClause) = do
  forM_ guardedClauses $ \(gd, pr) -> do
    e <- checkExpr ctx gd
    unless (e == TyBool) $ throwError $
      TypeError "guard condition needs to be of type bool"
    checkProgram ctx pr
  case mElseClause of
    Nothing -> pure TyUnit
    Just elseClause -> checkProgram ctx elseClause >> pure TyUnit

checkExpr :: Monad m => TypeContextT m -> Expr -> ExceptT TypeError m Ty
checkExpr _ (IntLiteral _) = pure TyInt
checkExpr _ (StringLiteral _) = pure TyString
checkExpr _ (ContextMacro []) = throwError $ TypeError "undefined context macro"
checkExpr ctx (ContextMacro (name:path)) = do
  tyContextMacro <- lookupTyContextMacro ctx name
  lookupTy tyContextMacro path
checkExpr ctx (BinaryOp "==" e1 e2) = do
  t1 <- checkExpr ctx e1
  t2 <- checkExpr ctx e2
  if t1 == t2
    then pure TyBool
    else throwError $ TypeError $
         "type of expression " ++ show e1 ++ "(" ++ show t1 ++ ") " ++
         "does not match that of " ++ show e2 ++ "(" ++ show t2 ++ ")"
checkExpr _ (BinaryOp op _ _) =
  throwError $ TypeError $ "unknown binary operation " ++ op


