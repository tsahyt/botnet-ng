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
import Network.Voco hiding (nick, user)
import System.Exit
import System.FilePath

import qualified Data.Acid as Acid

import Components.Admin
import Components.Citation
import Components.Crates
import Components.Github
import Components.Interject
import Components.Market
import Components.Markov
import Components.Manual
import Components.Misc
import Components.Permission
import Components.Search
import Components.UserData
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
    , serverPass = pwd
    , botUser = user
    , botRealname = "bot"
    , botNickname = nick
    }

type States = '[ UserPermissions, Combos]

initAcid :: Config -> IO (AcidStates States)
initAcid Config {..} = do
    uperms <-
        Acid.openLocalStateFrom (dbRoot paths </> "user-permissions") mempty
    combs <-
        Acid.openLocalStateFrom (dbRoot paths </> "combos") mempty
    -- grant root all permissions on start
    mapM_ (\p -> Acid.update uperms $ GrantU root p) allPerms
    pure $ uperms :+ combs :+ NullState

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
            let server = configServer (connection c)
                nt = NT $ \x -> runReaderT (runAcidT x states) env
            botloop server nt (standard (channels c) <> bot)
  where
    bot =
        (irc $
         permissions <|> search <|> citations <|> markov <|> github <|> crates <|>
         markets <|> agencies <|> interject <|> userData [] <|> combos <|> man <|>
         admin) <>
        (irc wolfram) <>
        (irc $ source <|> help)
