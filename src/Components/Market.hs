-- | A stock ticker component
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Components.Market
    ( markets
    , stock
    , crypto
    ) where

import Control.Lens
import Control.Monad.Reader
import Control.Applicative
import Data.Aeson.Lens
import Data.Config
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.Voco
import Network.Wreq
import Network.Yak.Client
import Network.Yak.Types (Message(..))
import Text.Read
import Text.Printf

import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

markets ::
       (MonadIO m, MonadChan m, HasConfig r, MonadReader r m)
    => Bot m Privmsg ()
markets = stock <|> crypto

stock ::
       (MonadIO m, MonadChan m, HasConfig r, MonadReader r m)
    => Bot m Privmsg ()
stock =
    answeringP $ \src ->
        on (view _Wrapped) .
        parsed (A.string ":stock" *> A.skipSpace *> A.takeText) .
        withKey alphaVantage $ \k -> asyncV $ do
            sym <- query
            r <- avStock k sym
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

avStock :: MonadIO m => Text -> Text -> m (Maybe StockQuote)
avStock k sym = do
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

crypto ::
       (MonadIO m, MonadChan m, HasConfig r, MonadReader r m)
    => Bot m Privmsg ()
crypto =
    answeringP $ \src ->
        on (view _Wrapped) . parsed cryptoP . withKey alphaVantage $ \k -> 
            asyncV $ do
                (sym, market) <- query
                r <- avCrypto k sym (fromMaybe "USD" market)
                case r of
                    Nothing -> message' src "Error while fetching quote"
                    Just q -> message' src . Message . fmtCrypto $ q

cryptoP :: A.Parser (Text, Maybe Text)
cryptoP =
    (A.string ":crypto" <|> A.string ":cc") *> A.skipSpace *>
    ((,) <$> sym <*> ((() <$ A.char '/' <|> A.skipSpace) *> optional sym))
  where
    sym = T.pack <$> A.many1 A.letter

fmtCrypto :: CryptoQuote -> Text
fmtCrypto CryptoQuote {..} =
    T.pack $
    printf
        ("%s: \002%s\x0F/\002%s\x0F @\003" <>
         "2 %s\003 %s")
        cryptoName
        cryptoSymbol
        cryptoMarket
        priceMarket
        cryptoMarket

data CryptoQuote = CryptoQuote
    { cryptoSymbol :: Text
    , cryptoName :: Text
    , cryptoMarket :: Text
    , priceMarket :: Text
    , cryptoDate :: Text
    } deriving (Show, Eq)

avCrypto :: MonadIO m => Text -> Text -> Text -> m (Maybe CryptoQuote)
avCrypto k sym market = do
    let opts =
            defaults & param "function" .~ ["CURRENCY_EXCHANGE_RATE"] &
            param "from_currency" .~
            [sym] &
            param "to_currency" .~
            [market] &
            param "apikey" .~
            [k]
    r <- liftIO $ getWith opts "https://www.alphavantage.co/query"
    let body = responseBody . key "Realtime Currency Exchange Rate"
        quote = CryptoQuote <$> pure sym <*> (r ^? body . key "2. From_Currency Name" . _String) <*> pure market <*> (r ^? body . key "5. Exchange Rate" . _String) <*> (r ^? body . key "6. Last Refreshed" . _JSON)
    pure quote
