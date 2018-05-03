{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Haxl.JSON where

import TreeLang.Syntax

data GenericDataSourceSettings = GenericDataSourceSettings
  { value :: String
  , ty :: String
  }

data GenericDataSource a where
  GenericValue :: GenericDataSource Expr

newGenericDataSource = undefined

deriving instance Eq (GenericDataSource a)
