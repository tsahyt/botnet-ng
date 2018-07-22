{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Acid
import Data.Config
import Data.Monoid ((<>))
import Network.Voco hiding (nick)
import Network.Yak.Client hiding (hostname, nick)
import System.Exit
import System.FilePath

import qualified Data.Acid as Acid

import Components.Permission

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

type States = '[UserPermissions]

initAcid :: Config -> IO (AcidStates States)
initAcid Config {..} = do
    uperms <- Acid.openLocalStateFrom (dbroot </> "user-permissions") mempty
    -- grant root all permissions on start
    mapM_ (\p -> Acid.update uperms $ GrantU root p) allPerms
    pure $ uperms :+ NullState

main :: IO ()
main = do
    cli <- parseCLI
    c <- readConfig (unHelpful $ config cli)
    case c of
        Left e -> print e >> exitFailure
        Right config -> do
            states <- initAcid config
            let server = configServer (connection $ config)
                nt = NT $ \x -> runAcidT x states
            botloop server nt (standard (channels config) <> bot)
  where
    bot = irc useful <> irc permissions

useful ::
       (AcidMember UserPermissions s, MonadAcid s m, MonadChan m)
    => Bot m Privmsg ()
useful =
    answeringP $ \_ ->
        filterB (== "!jlaw") $ do
            message
                "#voco-example"
                "if you people hadn't kicked jlaw already, I would do it now"
