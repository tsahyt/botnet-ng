{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Config
import Control.Category
import Network.Voco
import Network.Yak.Client hiding (hostname)
import Data.Monoid ((<>))
import System.Exit

import Prelude hiding (id, (.))

configServer :: ConnectionConfig -> IRCServer
configServer ConnectionConfig{..} =
    IRCServer
    { serverConnectionParams =
        ConnectionParams
            { connectionHostname = hostname
            , connectionPort = port
            , connectionUseSecure =
                if ssl then Just $ TLSSettingsSimple False False False
                       else Nothing
            , connectionUseSocks = Nothing }
    , serverPass = Nothing
    , botUser = "bot"
    , botRealname = "bot"
    , botNickname = "botnet-ng"
    }

main :: IO ()
main = do
    cli <- parseCLI
    c <- readConfig (unHelpful $ config cli)
    case c of
        Left e -> print e >> exitFailure
        Right config -> do
            let server = configServer (connection $ config)
            botloop server id (standard (channels config) <> irc useful)

useful :: MonadChan m => Bot m Privmsg ()
useful = answeringP $ \_ -> filterB (== "!jlaw") $ do
    kick "#linuxmasterrace" "Jennifer-Lawrence" (Just "hi")
