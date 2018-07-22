{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Category
import Network.Voco
import Network.Yak.Client
import Data.Monoid ((<>))

import Prelude hiding (id, (.))

server :: IRCServer
server =
    IRCServer
    { serverConnectionParams =
        ConnectionParams
            { connectionHostname = "irc.snoonet.org"
            , connectionPort = 6697
            , connectionUseSecure = Just $ TLSSettingsSimple False False False
            , connectionUseSocks = Nothing }
    , serverPass = Nothing
    , botUser = "bot"
    , botRealname = "bot"
    , botNickname = "botnet-ng"
    }

main :: IO ()
main = botloop server id (standard ["#linuxmasterrace"] <> irc useful)

useful :: MonadChan m => Bot m Privmsg ()
useful = answeringP $ \_ -> filterB (== "!jlaw") $ do
    kick "#linuxmasterrace" "Jennifer-Lawrence" (Just "hi")
