{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Components.UserData
    ( UserData(..)
    , userData
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson.Encoding.Internal
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Voco
import Network.Yak
import Network.Yak.Client
import Network.Wreq

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

data UserData = forall a. ToJSON a =>
                            UserData
    { dumpData :: Host -> IO a
    , deleteData :: Host -> IO ()
    }

dumpData' :: Host -> UserData -> IO Encoding
dumpData' h (UserData d _) = toEncoding <$> d h

merge :: [Encoding] -> Encoding
merge = Encoding . mconcat . map fromEncoding

data UserDataQ
    = DeleteData
    | DumpData

userDataP :: A.Parser UserDataQ
userDataP =
    A.choice
        [ DeleteData <$ A.string ":delete-data"
        , DumpData <$ A.string ":dump-data"
        ]

confirmCode :: MonadRandom m => m Text
confirmCode = T.pack <$> replicateM 8 (getRandomR ('0', '}'))

origin :: Prefix -> Maybe Host
origin (PrefixUser h) = Just h
origin _ = Nothing

userData ::
       forall m. (MonadChan m, MonadIO m, MonadRandom m)
    => [UserData]
    -> Bot m Privmsg ()
userData xs = do
    p <- msgPrefix <$> query
    case p >>= origin of
        Nothing -> abort
        Just host -> go host
  where
    go :: Host -> Bot m Privmsg ()
    go host =
        answeringP $ \src ->
            on (view _Wrapped) . parsed userDataP $
            query >>= \case
                DeleteData -> do
                    a <-
                        async $ do
                            rcode <- confirmCode
                            _ <-
                                request $ do
                                    message' src (confirmMessage rcode)
                                    recvG (verify rcode host)
                            liftIO $ mapM_ (flip deleteData host) xs
                            message' src "Data deleted!"
                    timeoutV (minutes 1) a
                DumpData -> do
                    url <- liftIO $ do
                        x <- fmap merge $ mapM (dumpData' host) xs
                        postData . toStrict . encodingToLazyByteString $ x
                    messageUser (host ^. hostNick) $
                        "Your data is available at " <> Message url <> 
                        " and will expire after first download or one day!"

postData :: ByteString -> IO Text
postData x = do
    let opts = defaults & header "Max-Downloads" .~ ["1"]
                        & header "Max-Days" .~ ["1"]
    r <- putWith opts "https://transfer.sh/userdata.json" x
    print r
    case r ^? responseBody of
        Nothing -> pure "<ERROR DURING UPLOAD>"
        Just u -> pure . decodeUtf8 . toStrict $ u

verify :: Text -> Host -> Privmsg -> Bool
verify rcode host m =
    case origin <$> msgPrefix m of
        Just (Just h)
            | h == host -> m ^. privmsgMessage . _Wrapped == rcode
        _ -> False

confirmMessage :: Text -> Message
confirmMessage rcode =
    "Do you really want to delete all your data? This cannot be undone!\
    \ To proceed, say " <> Message rcode <> " within one minute"
