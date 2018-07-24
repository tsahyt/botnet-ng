{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Components.Misc (
    source
) where

import Data.Coproduct
import Network.Voco
import Network.Yak.Client

-- TODO: pull repo from cabal file
-- TODO: fix answering, doesn't respond in PM
source :: MonadChan m => Bot m (Privmsg :|: Notice) ()
source = answering $ \src -> filterB (== ":source") $
    message' src "https://github.com/tsahyt/botnet-ng"
