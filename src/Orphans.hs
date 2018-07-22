-- | botnet-ng Orphan instances
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Network.Yak.Types
import Network.Voco
import Data.SafeCopy
import Data.Yaml

deriveSafeCopy 0 'base ''Host

instance FromJSON Host
instance ToJSON Host
instance FromJSON PortNumber where
    parseJSON (Number s) =
        let x = truncate s
         in pure $ fromInteger x
    parseJSON _ = fail "unable to parse port"
instance FromJSON Channel where
    parseJSON = fmap Channel . parseJSON
