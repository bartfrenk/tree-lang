{-# LANGUAGE FlexibleContexts #-}
module TreeLang.Checker where

import Data.Functor.Identity
import Control.Monad.Except
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import TreeLang.Syntax

data TyContextT m =
  TyContextT (Map Name (m Ty))

newTyContext :: [(Name, m Ty)] -> TyContextT m
newTyContext = TyContextT . Map.fromList

emptyTypeContext :: TyContextT Identity
emptyTypeContext = TyContextT Map.empty

lookupTy :: Monad m => TyContextT m -> Name -> ExceptT TypeError m Ty
lookupTy (TyContextT ctx) name = case ctx !? name of
    Nothing -> throwError $ TypeError $ "could not find context object " ++ name
    Just obj -> ExceptT $ Right <$> obj


data Ty
  = TyInt
  | TyString
  | TyBool
  | TyUnit
  | TyFloat
  | TyRecord (Map Name Ty)
  | TyFunction Ty Ty
  deriving (Eq, Show)

newTyRecord :: [(Name, Ty)] -> Ty
newTyRecord = TyRecord . Map.fromList

isNullTyRecord :: Ty -> Bool
isNullTyRecord (TyRecord attrs) = Map.null attrs
isNullTyRecord _ = False

data TypeError
  = TypeError { unTypeError :: String }
  deriving (Show)

checkProgram :: Monad m => TyContextT m -> Program -> ExceptT TypeError m Ty
checkProgram ctx program = last <$> forM program (checkStatement ctx)

checkStatement :: Monad m => TyContextT m -> Statement -> ExceptT TypeError m Ty
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

checkExpr :: Monad m => TyContextT m -> Expr -> ExceptT TypeError m Ty
checkExpr _ (IntLiteral _) = pure TyInt
checkExpr _ (StringLiteral _) = pure TyString
checkExpr _ (FloatLiteral _) = pure TyFloat
checkExpr _ (BoolLiteral _) = pure TyBool
checkExpr ctx e@(ContextMacro name params) = do
  t1 <- checkExpr ctx params
  t2 <- lookupTy ctx name
  case (t1, t2) of
    (TyRecord _, TyFunction dom range) ->
      if t1 == dom then pure range
      else throwError $ TypeError $
             "expr " ++ show e ++ " expects argument of type " ++ show dom
    (TyRecord _, _) ->
      if isNullTyRecord t1 then pure t2
      else throwError $ TypeError $
           "expr " ++ show e ++ " does not expect arguments"
    (_, _) -> throwError $ TypeError $
                "expr " ++ show e ++ " requires named arguments" ++ show t1
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

