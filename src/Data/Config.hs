{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Data.Config
    -- * Configuration File
    ( readConfig
    , Config(..)
    , ConnectionConfig(..)
    -- * Command Line
    , parseCLI
    , CLI(..)
    -- * Re-Export
    , unHelpful
    ) where

import Control.Monad.IO.Class
import Data.Yaml
import GHC.Generics
import Data.List.NonEmpty (NonEmpty)
import Network.Voco (HostName, PortNumber)
import Network.Yak.Types (Channel, Host, Nickname)
import Options.Generic
import Orphans ()

data Config = Config
    { root :: Host
    , connection :: ConnectionConfig
    , channels :: NonEmpty Channel
    , nick :: Nickname
    } deriving (Generic, FromJSON)

data ConnectionConfig = ConnectionConfig
    { hostname :: HostName
    , port :: PortNumber
    , ssl :: Bool
    } deriving (Generic, FromJSON)

readConfig :: MonadIO m => FilePath -> m (Either ParseException Config)
readConfig = liftIO . decodeFileEither

data CLI = CLI
    { config :: FilePath <?> "Path to configuration file"
    } deriving (Generic, ParseRecord)

parseCLI :: MonadIO m => m CLI
parseCLI = getRecord "botnet-ng"
