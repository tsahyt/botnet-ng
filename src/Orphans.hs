-- | botnet-ng Orphan instances
{-# LANGUAGE TemplateHaskell #-}
module Orphans where

import Network.Yak.Types
import Data.SafeCopy

deriveSafeCopy 0 'base ''Host
