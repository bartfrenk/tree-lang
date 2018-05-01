{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module TreeLang.Haxl.Weather where

-- import Haxl.Core
-- import Data.Hashable
-- import Control.Monad.Trans (MonadIO, liftIO)
-- import Control.Monad.Except

-- import TreeLang.Context
-- import TreeLang.Syntax
-- import TreeLang.Checker
-- import TreeLang.Haxl
-- import TreeLang.Haxl.Location


-- data WeatherSource a where
--   WeatherValue :: String -> String -> WeatherSource (ContextObj Expr)
--   WeatherType :: WeatherSource (ContextObj Ty)

-- deriving instance Eq (WeatherSource a)

-- instance Hashable (WeatherSource a) where
--   hashWithSalt s (WeatherValue cc city) = hashWithSalt s (0 :: Int, cc, city)
--   hashWithSalt s WeatherType = hashWithSalt s (1 :: Int)

-- deriving instance Show (WeatherSource a)

-- instance ShowP WeatherSource where
--   showp = show

-- instance StateKey WeatherSource where
--   data State WeatherSource = UserState {}

-- instance DataSourceName WeatherSource where
--   dataSourceName _ = "WeatherSource"

-- instance DataSource u WeatherSource where
--   fetch _state _flags _userEnv blockedFetches = SyncFetch $ do
--     unless (null objVars) $ do
--       vals <- mapM (uncurry fetchWeatherValue) locs
--       mapM_ (uncurry putSuccess) (zip objVars vals)

--     unless (null tyVars) $ do
--       ty <- fetchWeatherType
--       mapM_ (\v -> putSuccess v ty) tyVars
--     where
--       locs :: [(String, String)]
--       objVars :: [ResultVar (ContextObj Expr)]
--       (locs, objVars) = unzip
--         [((cc, city), var) | BlockedFetch (WeatherValue cc city) var <- blockedFetches]
--       tyVars :: [ResultVar (ContextObj Ty)]
--       tyVars = [var | BlockedFetch WeatherType var <- blockedFetches]

-- _tyWeather :: ContextObj Ty
-- _tyWeather = newContextObj
--   [ ("temperature", TyFloat)
--   , ("conditions", TyString)
--   ]

-- _valWeather :: ContextObj Expr
-- _valWeather = newContextObj
--   [ ("temperature", FloatLiteral 0.0)
--   , ("conditions", StringLiteral "rainy")
--   ]

-- fetchWeatherType :: MonadIO m => m (ContextObj Ty)
-- fetchWeatherType = do
--   liftIO $ putStrLn "fetching weather type"
--   pure _tyWeather

-- fetchWeatherValue :: MonadIO m => String -> String -> m (ContextObj Expr)
-- fetchWeatherValue cc city = do
--   liftIO $ putStrLn $ "fetching weather value for " ++ city ++ " (" ++ cc ++ ")"
--   pure _valWeather

-- tyWeather :: Haxl (ContextObj Ty)
-- tyWeather = dataFetch WeatherType

-- valWeather :: Haxl (ContextObj Expr)
-- valWeather = do
--   loc <- valLocation
--   cc' <- runExceptT (lookupAtom loc ["countryCode"])
--   city' <- runExceptT (lookupAtom loc ["city"])
--   case (cc', city') of
--     (Right (StringLiteral cc), Right (StringLiteral city)) ->
--       dataFetch (WeatherValue cc city)
--     _ -> throw $ TransientError (UnexpectedType "location")
