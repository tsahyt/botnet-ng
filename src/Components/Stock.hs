-- | A stock ticker component
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Components.Stock
    ( stock
    ) where

import Control.Lens
import Control.Monad.Reader
import Data.Aeson.Lens
import Data.Config
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.Voco
import Network.Wreq
import Network.Yak.Client
import Network.Yak.Types (Message(..))
import Text.Read
import Text.Printf

import qualified Data.Attoparsec.Text as A

stock :: (MonadIO m, MonadChan m, MonadReader Config m) => Bot m Privmsg ()
stock =
    answeringP $ \src ->
        on (view _Wrapped) .
        parsed (A.string ":stock" *> A.skipSpace *> A.takeText) .
        withKey alphaVantage $ \k -> do
            sym <- query
            r <- avReq k sym
            case r of
                Nothing -> message' src "Error while fetching quote"
                Just q -> message' src . Message . fmtQuote $ q

fmtQuote :: StockQuote -> Text
fmtQuote StockQuote {..} =
    T.pack $
    printf
        ("\002%s\x0F -- Close at\003" <>
         "2 %.2f\003. O: %.2f, H: %.2f, L: %.2f (\002%s\x0F)")
        symbol
        close
        open
        high
        low
        quoteDate

data StockQuote = StockQuote
    { symbol :: Text
    , open :: Double
    , high :: Double
    , low :: Double
    , close :: Double
    , quoteDate :: Text
    } deriving (Show, Eq)

avReq :: MonadIO m => Text -> Text -> m (Maybe StockQuote)
avReq k sym = do
    let opts =
            defaults & param "function" .~ ["TIME_SERIES_DAILY"] &
            param "symbol" .~
            [sym] &
            param "apikey" .~
            [k]
    r <- liftIO $ getWith opts "https://www.alphavantage.co/query"
    let quote = do
            let today =
                    T.takeWhile
                        (/= ' ')
                        (r ^. responseBody . key "Meta Data" .
                         key "3. Last Refreshed" .
                         _String)
            dat <-
                preview (responseBody . key "Time Series (Daily)" . key today) r
            o <- readMaybe . T.unpack $ dat ^. key "1. open" . _String
            h <- readMaybe . T.unpack $ dat ^. key "2. high" . _String
            l <- readMaybe . T.unpack $ dat ^. key "3. low" . _String
            c <- readMaybe . T.unpack $ dat ^. key "4. close" . _String
            pure $ StockQuote sym o h l c today
    pure quote
