{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Components.Wolfram
    ( wolfram
    , Conversations
    , emptyConversations
    , HasConversations(..)
    ) where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader
import Data.Aeson.Lens
import Data.Config
import Data.Coproduct
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Network.Voco
import Network.Wreq
import Network.Yak
import Network.Yak.Client

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

type ConvId = Text

newtype Conversations =
    Conversations (TVar (Map Nickname (UTCTime, ConvId, Text)))
    deriving (Eq)

emptyConversations :: MonadIO m => m Conversations
emptyConversations = Conversations <$> liftIO (newTVarIO mempty)

makeClassy ''Conversations

makeWrapped ''Conversations

wolfram ::
       ( HasConversations r
       , MonadReader r m
       , MonadIO m
       , MonadChan m
       , HasConfig r
       )
    => Bot m Privmsg ()
wolfram =
    msgPrefix <$> query >>= \case
        Just (PrefixUser (Host n _ _)) ->
            answeringP $ \src -> on (view _Wrapped) (conversation n src)
        _ -> abort

conversation ::
       ( MonadReader r m
       , HasConversations r
       , MonadIO m
       , MonadChan m
       , HasConfig r
       )
    => Nickname
    -> Channel :|: Nickname -> Bot m Text ()
conversation nn src = do
    env <- ask
    asyncV' (NT $ flip runReaderT env) $
        parsed (A.string ":hal" *> A.skipSpace *> A.takeText) $ do
            question <- query
            previous <-
                do tv <- view $ conversations . _Wrapped
                   cs <- liftIO . atomically . readTVar $ tv
                   pure $ cs ^. at nn
            now <- liftIO getCurrentTime
            ans <-
                case previous of
                    Nothing -> fresh question now
                    Just (lastTime, convId, host :: Text)
                        | diffUTCTime now lastTime > 180 -> fresh question now
                        | otherwise -> continue question now convId host
            message' src (Message $ nn <> ": " <> ans)
  where
    fresh question now =
        withKey wolframAlpha $ \k -> do
            let opts = defaults & param "appid" .~ [k] & param "i" .~ [question]
            r <-
                liftIO $
                getWith opts "https://api.wolframalpha.com/v1/conversation.jsp"
            let convId = r ^. responseBody . key "conversationID" . _String
                host = r ^. responseBody . key "host" . _String
                ans = r ^. responseBody . key "result" . _String
                err = r ^. responseBody . key "error" . _String
            case err of
                "" -> do
                    tv <- view $ conversations . _Wrapped
                    liftIO . atomically . modifyTVar tv $ at nn .~
                        Just (now, convId, host)
                    pure ans
                _ -> pure "I'm afraid I can't do that"
    continue question now convId host =
        withKey wolframAlpha $ \k -> do
            let opts =
                    defaults & param "appid" .~ [k] & param "i" .~ [question] &
                    param "conversationid" .~
                    [convId]
            r <-
                liftIO $
                getWith
                    opts
                    ("https://" <> T.unpack host <> "/api/v1/conversation.jsp")
            tv <- view $ conversations . _Wrapped
            let convId' = r ^. responseBody . key "conversationID" . _String
                host' = r ^. responseBody . key "host" . _String
                ans = r ^. responseBody . key "result" . _String
                err = r ^. responseBody . key "error" . _String
            case err of
                "" -> do
                    liftIO . atomically . modifyTVar tv $ at nn .~
                        Just (now, convId', host')
                    pure ans
                _ -> do
                    liftIO . atomically . modifyTVar tv $ at nn .~ Nothing
                    pure err
