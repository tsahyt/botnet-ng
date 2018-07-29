-- | Bots providing search functionality
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Components.Search
    ( search
    ) where

import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Text (Text)
import Network.Voco
import Network.Wreq
import Network.Yak.Client
import Network.Yak.Types (Message(..))

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

search :: (MonadIO m, MonadChan m) => Bot m Privmsg ()
search = ddg 200

ddg :: (MonadIO m, MonadChan m) => Int -> Bot m Privmsg ()
ddg limit =
    answeringP $ \src ->
        on (view _Wrapped) .
        parsed (A.string ":ddg" *> A.skipSpace *> A.takeText) . asyncV $ do
            r <- ddGo =<< query
            case ddgProcess limit <$> r of
                Just (TextAnswer answer url) ->
                    message'
                        src
                        (Message $ answer <>
                         if not (T.null url)
                             then " (" <> url <> ")"
                             else mempty)
                Just (ImageAnswer img) ->
                    message' src (Message $ "Found an image" <> img)
                Just (BangAnswer redir) -> message' src (Message redir)
                Just (OnlyURL url) -> message' src (Message url)
                _ -> message' src "I cannot search for that!"

data DDGAnswer
    = TextAnswer Text
                 Text
    | OnlyURL Text
    | BangAnswer Text
    | ImageAnswer Text
    | Invalid

ddGo :: MonadIO m => Text -> m (Maybe (Response ByteString))
ddGo q = do
    let opts =
            defaults & param "format" .~ ["json"] & param "q" .~ [q] &
            param "no_redirect" .~
            ["1"] &
            redirects .~
            0
    r <- liftIO . try @SomeException $ getWith opts "https://api.duckduckgo.com"
    pure . either (const Nothing) Just $ r

ddgProcess :: Int -> Response ByteString -> DDGAnswer
ddgProcess limit r
    | T.null answer && not (T.null redir) = BangAnswer redir
    | T.null answer && not (T.null img) = ImageAnswer img
    | T.null answer && not (T.null url) = OnlyURL url
    | not (T.null answer) = TextAnswer answer url
    | otherwise = Invalid
  where
    answer =
        let a = T.stripStart $ r ^. responseBody . key "AbstractText" . _String
        in if T.length a > limit
               then T.stripEnd (T.take limit a) <> "..."
               else a
    url = r ^. responseBody . key "AbstractURL" . _String
    redir = r ^. responseBody . key "Redirect" . _String
    img = r ^. responseBody . key "Image" . _String
