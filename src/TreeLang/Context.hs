{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module TreeLang.Context where

import Control.Monad.Except
import Control.Arrow (left, second)
import Data.List (intercalate)
import Data.Map

type Name = String

data ContextObj a
  = Atom a
  | Obj (Map Name (ContextObj a))

deriving instance Show a => Show (ContextObj a)

newContextObj :: [(Name, a)] -> ContextObj a
newContextObj = Obj . fromList . fmap (second Atom)

newContext :: Applicative m => [(Name, m (ContextObj a))] -> ContextT a m
newContext =
  ContextT . fromList

data ContextT a m =
  ContextT (Map Name (m (ContextObj a)))

lookupObj :: Monad m => ContextT a m -> Name -> ExceptT String m (ContextObj a)
lookupObj (ContextT ctx) name = case ctx !? name of
    Nothing -> throwError $ "could not find context object \"" ++ name ++ "\""
    Just obj -> ExceptT $ Right <$> obj

lookupAtom :: Monad m => ContextObj a -> [Name] -> ExceptT String m a
lookupAtom obj path = lookupAtom' obj path
  where
    lookupAtom' (Atom x) [] = pure x
    lookupAtom' (Obj _) [] =
      throwError $ showP path ++ " does not refer to an atom"
    lookupAtom' (Atom _) _ =
      throwError $ showP path ++ " too long"
    lookupAtom' (Obj m) (n:ns) =
      case m !? n of
        Nothing ->
          throwError $ showP path ++ " not in object"
        Just obj' ->
          lookupAtom' obj' ns
    showP = intercalate "."

toError :: Monad m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
toError f = mapExceptT (\ee -> left f <$> ee)
