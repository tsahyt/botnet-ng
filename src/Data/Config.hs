{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Config
    -- * Configuration File
    ( readConfig
    , Config(..)
    , Paths(..)
    , ConnectionConfig(..)
    , APIKeys(..)
    -- * Command Line
    , parseCLI
    , CLI(..)
    -- * Re-Export
    , unHelpful
    -- * Combinators
    , withKey
    ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Yaml
import Data.Char
import Data.Aeson (genericParseJSON, Options(..), defaultOptions)
import GHC.Generics
import Data.List.NonEmpty (NonEmpty)
import Network.Voco
import Network.Yak.Types (Channel, Host, Nickname)
import Options.Generic
import Orphans ()

applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []     = []
applyFirst f [x]    = [f x]
applyFirst f (x:xs) = f x: xs

snakeCase :: String -> String
snakeCase = u . applyFirst toLower
  where
    u [] = []
    u (x:xs)
        | isUpper x = '-' : toLower x : snakeCase xs
        | otherwise = x : u xs

data Config = Config
    { root :: Host
    , connection :: ConnectionConfig
    , channels :: NonEmpty Channel
    , nick :: Nickname
    , paths :: Paths
    , keys :: APIKeys
    } deriving (Generic, FromJSON)

data Paths = Paths
    { dbRoot :: FilePath
    , citationRoot :: FilePath
    } deriving (Generic)

instance FromJSON Paths where
    parseJSON =
        genericParseJSON $ defaultOptions {fieldLabelModifier = snakeCase}

data ConnectionConfig = ConnectionConfig
    { hostname :: HostName
    , port :: PortNumber
    , ssl :: Bool
    } deriving (Generic, FromJSON)

data APIKeys = APIKeys
    { alphaVantage :: Maybe Text }
    deriving (Generic)

instance FromJSON APIKeys where
    parseJSON =
        genericParseJSON $ defaultOptions {fieldLabelModifier = snakeCase}

withKey ::
       MonadReader Config m
    => (APIKeys -> Maybe key)
    -> (key -> Bot m i o)
    -> Bot m i o
withKey f g = do
    k <- reader (f . keys)
    case k of
        Nothing -> abort
        Just k' -> g k'

readConfig :: MonadIO m => FilePath -> m (Either ParseException Config)
readConfig = liftIO . decodeFileEither

data CLI = CLI
    { config :: FilePath <?> "Path to configuration file"
    } deriving (Generic, ParseRecord)

parseCLI :: MonadIO m => m CLI
parseCLI = getRecord "botnet-ng"
