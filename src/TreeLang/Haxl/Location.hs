{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeLang.Haxl.Location where

import Control.Monad
import Haxl.Core
import Data.Hashable
import Control.Monad.Trans (MonadIO, liftIO)

import TreeLang.Context
import TreeLang.Syntax
import TreeLang.Checker
import TreeLang.Haxl


data LocationSource a where
  LocationValue :: LocationSource (ContextObj Expr)
  LocationType :: LocationSource (ContextObj Ty)

deriving instance Eq (LocationSource a)

instance Hashable (LocationSource a) where
  hashWithSalt s LocationValue = hashWithSalt s (0 :: Int)
  hashWithSalt s LocationType = hashWithSalt s (1 :: Int)

deriving instance Show (LocationSource a)

instance ShowP LocationSource where
  showp = show

instance StateKey LocationSource where
  data State LocationSource = UserState {}

instance DataSourceName LocationSource where
  dataSourceName _ = "LocationSource"

instance DataSource u LocationSource where
  fetch _state _flags _userEnv blockedFetches = SyncFetch $ do
    unless (null objVars) $ do
      val <- fetchLocationValue
      mapM_ (\v -> putSuccess v val) objVars

    unless (null tyVars) $ do
      ty <- fetchLocationType
      mapM_ (\v -> putSuccess v ty) tyVars
    where
      objVars :: [ResultVar (ContextObj Expr)]
      objVars = [var | BlockedFetch LocationValue var <- blockedFetches]
      tyVars :: [ResultVar (ContextObj Ty)]
      tyVars = [var | BlockedFetch LocationType var <- blockedFetches]

_tyLocation :: ContextObj Ty
_tyLocation = newContextObj
  [ ("country", TyString)
  , ("city", TyString)
  , ("countryCode", TyString)
  ]

_valLocation :: ContextObj Expr
_valLocation = newContextObj
  [ ("country", StringLiteral "Netherlands")
  , ("city", StringLiteral "Eindhoven")
  , ("countryCode", StringLiteral "NL")
  ]

fetchLocationType :: MonadIO m => m (ContextObj Ty)
fetchLocationType = do
  liftIO $ putStrLn "fetching location type"
  pure _tyLocation

fetchLocationValue :: MonadIO m => m (ContextObj Expr)
fetchLocationValue = do
  liftIO $ putStrLn "fetching location value"
  pure _valLocation

tyLocation :: Haxl (ContextObj Ty)
tyLocation = dataFetch LocationType

valLocation :: Haxl (ContextObj Expr)
valLocation = dataFetch LocationValue

