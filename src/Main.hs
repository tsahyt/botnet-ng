{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Control.Monad.Acid
import Control.Monad.Reader
import Data.Config
import Data.Monoid ((<>))
import Network.Voco hiding (nick)
import System.Exit
import System.FilePath

import qualified Data.Acid as Acid

import Components.Admin
import Components.Citation
import Components.Interject
import Components.Market
import Components.Misc
import Components.Permission
import Components.Search
import Components.Wolfram

configServer :: ConnectionConfig -> IRCServer
configServer ConnectionConfig {..} =
    IRCServer
    { serverConnectionParams =
          ConnectionParams
          { connectionHostname = hostname
          , connectionPort = port
          , connectionUseSecure =
                if ssl
                    then Just $ TLSSettingsSimple False False False
                    else Nothing
          , connectionUseSocks = Nothing
          }
    , serverPass = Nothing
    , botUser = "bot"
    , botRealname = "bot"
    , botNickname = "botnet-ng"
    }

type States = '[ UserPermissions]

initAcid :: Config -> IO (AcidStates States)
initAcid Config {..} = do
    uperms <-
        Acid.openLocalStateFrom (dbRoot paths </> "user-permissions") mempty
    -- grant root all permissions on start
    mapM_ (\p -> Acid.update uperms $ GrantU root p) allPerms
    pure $ uperms :+ NullState

data Environment = Environment
    { _envConfig :: Config
    , _envConversations :: Conversations
    }

makeLenses ''Environment

instance HasConfig Environment where
    config = envConfig

instance HasConversations Environment where
    conversations = envConversations

mkEnvironment :: Config -> IO Environment
mkEnvironment c = Environment <$> pure c <*> emptyConversations

main :: IO ()
main = do
    cli <- parseCLI
    readConfig (unHelpful $ conf cli) >>= \case
        Left e -> print e >> exitFailure
        Right c -> do
            states <- initAcid c
            env <- mkEnvironment c
            let server = configServer (connection $ c)
                nt = NT $ \x -> runReaderT (runAcidT x states) env
            botloop server nt (standard (channels c) <> bot)
  where
    bot =
        (irc $
         permissions <|> search <|> markets <|> citations <|>
         agencies <|> interject <|> admin) <>
        (irc wolfram) <>
        (irc source)
