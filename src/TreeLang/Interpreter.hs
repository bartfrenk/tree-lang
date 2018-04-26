module TreeLang.Interpreter
  ( interpretProgram
  , RuntimeError(..)
  ) where

import Control.Monad.Except

import TreeLang.Syntax
import TreeLang.Context

data RuntimeError = RuntimeError { unRuntimeError :: String }

type InterpreterT m a = ExceptT RuntimeError m a

-- TODO: concatenation of results is inefficient
interpretProgram :: Monad m => ContextT Expr m -> Program -> InterpreterT m Program
interpretProgram _ [] = return []
interpretProgram ctx (first:rest) = do
  pr <- interpretStatement ctx first
  ((++) pr) <$> interpretProgram ctx rest

interpretExpr :: Monad m => ContextT Expr m -> Expr -> InterpreterT m Expr
interpretExpr _ e@(IntLiteral _) = pure e
interpretExpr _ e@(StringLiteral _) = pure e
interpretExpr _ e@(FloatLiteral _) = pure e
interpretExpr _ e@(BoolLiteral _) = pure e
interpretExpr ctx (BinaryOp op e1 e2) = do
  e1' <- interpretExpr ctx e1
  e2' <- interpretExpr ctx e2
  case operatorType op of
    Just Equality -> pure $ BoolLiteral (e1' == e2')
    Just Comparison -> do
      case (getDouble e1', getDouble e2') of
        (Just d1, Just d2) -> do
          f <- toHaskellOp op
          pure $ BoolLiteral (d1 `f` d2)
        _ -> throwError $
             RuntimeError $ "cannot compare " ++ (show e1') ++ " and " ++ (show e2')
    Nothing -> throwError $ RuntimeError $ "unknown operator type " ++ op
interpretExpr _ (ContextMacro []) = throwError $ RuntimeError "undefined context macro"
interpretExpr ctx (ContextMacro (name:path)) = do
  e <- toError RuntimeError $ do
    obj <- lookupObj ctx name
    lookupAtom obj path
  interpretExpr ctx e

interpretStatement :: Monad m => ContextT Expr m -> Statement -> InterpreterT m Program
interpretStatement ctx (Assignment name expr) = do
  e <- interpretExpr ctx expr
  pure [Assignment name e]
interpretStatement ctx (Cond guarded other) =
  let clauses = guarded ++ case other of
                             Nothing -> []
                             Just pr -> [(BoolLiteral True, pr)]
  in interpretCondClauses ctx clauses

interpretCondClauses :: Monad m
                     => ContextT Expr m -> [(Expr, Program)] -> InterpreterT m Program
interpretCondClauses _ [] = pure []
interpretCondClauses ctx ((expr, pr):rest) = do
  e <- interpretExpr ctx expr
  case e of
    BoolLiteral True -> interpretProgram ctx pr
    BoolLiteral False -> interpretCondClauses ctx rest
    _ -> throwError
      $ RuntimeError $ "guard " ++ show expr ++ " does not evaluate to boolean"


getDouble :: Expr -> Maybe Double
getDouble (IntLiteral i) = Just $ fromInteger i
getDouble (FloatLiteral d) = Just $ d
getDouble _ = Nothing

toHaskellOp :: Monad m => Ord a => String -> InterpreterT m (a -> a -> Bool)
toHaskellOp ">" = pure (>)
toHaskellOp "<" = pure (<)
toHaskellOp "<=" = pure (<=)
toHaskellOp ">=" = pure (>=)
toHaskellOp op = throwError $
  RuntimeError $ "undefined operator " ++ op
