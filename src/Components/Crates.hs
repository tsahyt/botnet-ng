{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.Crates
    ( crates
    ) where

import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Network.Voco
import Network.Wreq
import Network.Yak.Client
import Network.Yak.Types (Message(..))
import Text.Printf

import qualified Data.Attoparsec.Text as A

crates :: (MonadIO m, MonadChan m) => Bot m Privmsg ()
crates =
    answeringP $ \src ->
        on (view _Wrapped) . parsed cratesP . asyncV $ query >>= \case
            CrateQuery c ->
                getCrate c >>= \case
                    Left e -> message' src e
                    Right crate -> message' src . Message $ fmtCrate crate

newtype CrateQuery =
    CrateQuery Text

cratesP :: A.Parser CrateQuery
cratesP = CrateQuery <$> (A.string ":crate" *> A.skipSpace *> A.takeText)

data Crate = Crate
    { crateName :: Text
    , crateDescription :: Text
    , crateDocumentation :: Text
    } deriving (Show)

fmtCrate :: Crate -> Text
fmtCrate Crate {..} =
    pack $ printf fmt crateName crateDescription crateDocumentation
  where
    fmt = "%s - %s, docs at %s."

getCrate :: MonadIO m => Text -> m (Either Message Crate)
getCrate c = do
    let opts =
            defaults & header "User-Agent" .~ ["tsahyt/botnet-ng"] &
            checkResponse .~
            Nothing
        epnt = "https://crates.io/api/v1/crates/" <> unpack c
    liftIO . handle (\(_ :: SomeException) -> pure $ Left "Package not found!") $ do
        x <- getWith opts epnt
        let r' = x ^. responseBody
            crate =
                maybe (Left "Error in crates.io response") Right $ do
                    crateName <- r' ^? key "crate" . key "name" . _String
                    crateDescription <-
                        r' ^? key "crate" . key "description" . _String
                    crateDocumentation <-
                        r' ^? key "crate" . key "documentation" . _String
                    pure Crate {..}
        pure crate
