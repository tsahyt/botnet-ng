{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Acid
import Control.Monad.Reader
import Data.Config
import Data.Monoid ((<>))
import Network.Voco hiding (nick)
import System.Exit
import System.FilePath

import qualified Data.Acid as Acid

import Components.Citation
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

type States = '[UserPermissions, Citations]

initAcid :: Config -> IO (AcidStates States)
initAcid Config {..} = do
    uperms <-
        Acid.openLocalStateFrom (dbRoot paths </> "user-permissions") mempty
    cites <-
        Acid.openLocalStateFrom (dbRoot paths </> "citations") mempty
    -- grant root all permissions on start
    mapM_ (\p -> Acid.update uperms $ GrantU root p) allPerms
    pure $ uperms :+ cites :+ NullState

main :: IO ()
main = do
    cli <- parseCLI
    c <- readConfig (unHelpful $ config cli)
    case c of
        Left e -> print e >> exitFailure
        Right config -> do
            states <- initAcid config
            let server = configServer (connection $ config)
                nt = NT $ \x -> runReaderT (runAcidT x states) config
            botloop server nt (standard (channels config) <> bot)
  where
    bot = irc $ permissions <> citations
