{-# LANGUAGE FlexibleContexts #-}
module TreeLang.Checker where

import Data.Functor.Identity
import Control.Monad.Except
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe

import TreeLang.Syntax
import TreeLang.Context

import qualified TreeLang.Parser as P

data Ty
  = TyInt
  | TyString
  | TyBool
  | TyUnit
  | TyFloat
  | TyRecord (Map Name Ty)
  deriving (Eq, Show)

newTyRecord :: [(Name, Ty)] -> Ty
newTyRecord = TyRecord . Map.fromList

data TypeError
  = TypeError { unTypeError :: String }
  deriving (Show)

emptyTypeContext :: ContextT Ty Identity
emptyTypeContext = ContextT Map.empty

checkProgram :: Monad m => ContextT Ty m -> Program -> ExceptT TypeError m Ty
checkProgram ctx program = last <$> forM program (checkStatement ctx)

checkStatement :: Monad m => ContextT Ty m -> Statement -> ExceptT TypeError m Ty
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

checkExpr :: Monad m => ContextT Ty m -> Expr -> ExceptT TypeError m Ty
checkExpr _ (IntLiteral _) = pure TyInt
checkExpr _ (StringLiteral _) = pure TyString
checkExpr _ (FloatLiteral _) = pure TyFloat
checkExpr _ (BoolLiteral _) = pure TyBool
checkExpr _ (ContextMacro []) = throwError $ TypeError "undefined context macro"
checkExpr ctx (ContextMacro name) = toError TypeError $ lookupObj ctx name
checkExpr ctx (BinaryOp op e1 e2) = do
  t1 <- checkExpr ctx e1
  t2 <- checkExpr ctx e2
  case operatorType op of
    Just Equality ->
      if t1 == t2
      then pure TyBool
      else throwError $ TypeError $
           "type of expression " ++ show e1 ++ " (" ++ show t1 ++ ") " ++
           "does not match that of " ++ show e2 ++ " (" ++ show t2 ++ ")"
    Just Comparison ->
      if isNumeric t1 && isNumeric t2
      then pure TyBool
      else throwError $ TypeError $
           op ++ " only applies to numeric types"
    Just AttributeAccess ->
      case (t1, e2) of
        (TyRecord attrsTy, AttributeName name) -> do
          maybe
            (throwError $ TypeError $ "record " ++ show e1 ++
              " has no attribute " ++ show e2)
            pure (attrsTy !? name)
        _ -> throwError $ TypeError $
          "cannot access " ++ show e1 ++ " by " ++ show e2
    Nothing ->
      throwError $ TypeError $ "unknown binary operation " ++ op
checkExpr ctx (Record fields) =
  TyRecord <$> (mapM (checkExpr ctx) fields)
checkExpr _ (AttributeName _) = pure TyUnit

isNumeric :: Ty -> Bool
isNumeric TyFloat = True
isNumeric TyInt = True
isNumeric _ = False

