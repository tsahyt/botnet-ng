{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Github
    ( github
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Text (Text, pack, unpack)
import Data.Aeson.Lens
import Data.Monoid
import Data.Char
import Control.Exception
import Data.Time
import Network.Voco
import Network.Wreq
import Network.Yak.Client
import Network.Yak.Types (Message(..))
import Text.Printf

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

github :: (MonadIO m, MonadChan m) => Bot m Privmsg ()
github =
    answeringP $ \src ->
        on (view _Wrapped) . parsed githubP . asyncV $
        query >>= \case
            RepoQuery u r ->
                getRepo u r >>= \case
                    Left e -> message' src e
                    Right repo -> message' src . Message $ fmtRepo repo

data GithubQuery =
    RepoQuery Text
              Text

githubP :: A.Parser GithubQuery
githubP = A.string ":github" *> A.skipSpace *> repoQuery
  where
    repoQuery = RepoQuery <$> ident <*> (A.char '/' *> ident)
    ident = A.takeWhile1 (\x -> isDigit x || isAlpha x || x == '-')

data Repo = Repo
    { repoFullName :: Text
    , repoHtmlUrl :: Text
    , repoStargazersCount :: Integer
    , repoForksCount :: Integer
    , repoPushedAt :: UTCTime
    , repoLanguage :: Text
    , repoOpenIssues :: Integer
    , repoDescription :: Maybe Text
    } deriving (Eq, Show, Ord)

fmtRepo :: Repo -> Text
fmtRepo Repo {..} =
    pack $
    printf
        fmt
        repoHtmlUrl
        dsc
        (dat repoPushedAt)
        repoStargazersCount
        repoOpenIssues
        repoForksCount
        repoLanguage
  where
    fmt =
        "\003%s - %s\x0F - Last push on %s. Repository has %d stars,\
       \ %d open issues and %d forks. Written in %s."
    dat = formatTime defaultTimeLocale "%b %d, %Y"
    dsc =
        case repoDescription of
            Nothing -> "(No description)"
            Just x ->
                let x' = T.unwords . take 8 . T.words $ x
                in if x /= x'
                       then x' <> "[...]"
                       else x

getRepo :: MonadIO m => Text -> Text -> m (Either Message Repo)
getRepo u r = do
    let opts =
            defaults & header "User-Agent" .~ ["tsahyt/botnet-ng"] &
            checkResponse .~
            Nothing
        epnt = "https://api.github.com/repos/" <> unpack u <> "/" <> unpack r
    liftIO .
        handle (\(_ :: SomeException) -> 
                    pure $ Left "Repository not found!") $ do
        x <- getWith opts epnt
        let r' = x ^. responseBody
            repo =
                maybe (Left "Error parsing Github response") Right $ do
                    repoFullName <- r' ^? key "full_name" . _String
                    repoHtmlUrl <- r' ^? key "html_url" . _String
                    repoStargazersCount <-
                        r' ^? key "stargazers_count" . _Integer
                    repoForksCount <- r' ^? key "forks_count" . _Integer
                    repoPushedAt <- r' ^? key "pushed_at" . _JSON
                    repoLanguage <- r' ^? key "language" . _String
                    repoOpenIssues <- r' ^? key "open_issues" . _Integer
                    repoDescription <- r' ^? key "description" . _JSON
                    pure Repo {..}
        pure repo
