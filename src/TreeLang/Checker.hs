{-# LANGUAGE FlexibleContexts #-}
module TreeLang.Checker where

import Data.Functor.Identity
import Control.Monad.Except
import Control.Arrow (second)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import TreeLang.Syntax
import TreeLang.Context


data Ty
  = TyInt
  | TyString
  | TyBool
  | TyUnit
  | TyFloat
  | TyContextMacro (Map Name Ty) Ty
  deriving (Eq)

instance Show Ty where
  show TyInt = "int"
  show TyString = "string"
  show TyBool = "bool"
  show TyUnit = "unit"
  show TyFloat = "float"
  show (TyContextMacro params range)
    = "(" ++ showParams params ++ ") -> " ++ show range


type TypeContextT m = ContextT Ty m

data TypeError
  = TypeError { unTypeError :: String }
  deriving (Show)

emptyTypeContext :: TypeContextT Identity
emptyTypeContext = ContextT Map.empty

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

checkParams :: Monad m
            => TypeContextT m -> Map Name Ty -> Map Name Expr -> ExceptT TypeError m ()
checkParams ctx tys exprs = do
  when (Map.keysSet tys /= Map.keysSet exprs) $
    throwError $ TypeError $ "parameters do not match"
  mapM_ (checkKey . fst) (Map.toList tys)
    where checkKey key =
            let expr' = exprs ! key
                ty = tys ! key
            in do
              exprTy <- checkExpr ctx expr'
              when (ty /= exprTy) $ throwError $ TypeError $
                "type of parameter " ++ key ++ " should be " ++ show ty

checkExpr :: Monad m => TypeContextT m -> Expr -> ExceptT TypeError m Ty
checkExpr _ (IntLiteral _) = pure TyInt
checkExpr _ (StringLiteral _) = pure TyString
checkExpr _ (FloatLiteral _) = pure TyFloat
checkExpr _ (BoolLiteral _) = pure TyBool
checkExpr _ (ContextMacro [] _) = throwError $ TypeError "undefined context macro"
checkExpr ctx (ContextMacro (name:path) params) = do
  tyContextMacro <- toError TypeError $ lookupObj ctx name
  ty <- toError TypeError $ lookupAtom tyContextMacro path
  case ty of
    TyContextMacro t1 t2 -> do
      checkParams ctx t1 params
      pure t2
    t3 -> pure t3
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
    Nothing ->
      throwError $ TypeError $ "unknown binary operation " ++ op

isNumeric :: Ty -> Bool
isNumeric TyFloat = True
isNumeric TyInt = True
isNumeric _ = False


pureTypeContext :: Applicative m
                => Name -> [(Name, Ty)] -> [(Name, Ty)] -> ContextT Ty m
pureTypeContext name params fields = newContext
  [(name, pure $ newContextObj (second arrowType <$> fields))]
  where
    params' = Map.fromList params
    arrowType :: Ty -> Ty
    arrowType ty = TyContextMacro params' ty

