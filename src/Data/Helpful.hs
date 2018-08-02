{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.Helpful
    ( helpfulIRC
    , HelpfulBot
    , (<?>)
    , Help(..)
    ) where

import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Semigroup
import Data.Text (Text)
import Network.Voco
import Network.Yak
import Network.Yak.Client

import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as T

data HelpfulBot m i o = HelpfulBot
    { botHelp :: Map Text Text
    , unHelpful :: Bot m i o
    }

instance (Monoid o, Monad m) => Semigroup (HelpfulBot m i o) where
    HelpfulBot a b <> HelpfulBot c d = HelpfulBot (a <> c) (b `mappend` d)

instance (Monoid o, Monad m) => Monoid (HelpfulBot m i o) where
    mempty = HelpfulBot mempty mempty
    mappend = (<>)

instance Functor m => Functor (HelpfulBot m i) where
    fmap f (HelpfulBot h b) = HelpfulBot h (fmap f b)

instance Monad m => Applicative (HelpfulBot m i) where
    pure o = HelpfulBot mempty (pure o)
    HelpfulBot h1 f <*> HelpfulBot h2 a = HelpfulBot (h1 <> h2) (f <*> a)

instance Monad m => Alternative (HelpfulBot m i) where
    empty = HelpfulBot mempty empty
    HelpfulBot h1 a <|> HelpfulBot h2 b = HelpfulBot (h1 <> h2) (a <|> b)

data Help =
    Help Text
         Text

helpMap :: Help -> Map Text Text
helpMap (Help cmd htext) = Map.singleton cmd htext

(<?>) :: Help -> Bot m i o -> HelpfulBot m i o
h <?> b = HelpfulBot (helpMap h) b

infixr 2 <?>

helpfulIRC :: (Fetch i, MonadChan m) => HelpfulBot m i () -> Bot m ByteString ()
helpfulIRC b = irc (unHelpful b) <|> irc (mkHelpBot (botHelp b))

helpP :: A.Parser (Maybe Text)
helpP = A.string ":help" *> optional (A.skipSpace *> A.takeWhile1 (/= ' '))

mkHelpBot :: MonadChan m => Map Text Text -> Bot m Privmsg ()
mkHelpBot m =
    answeringP $ \src ->
        on (view _Wrapped) .
        parsed helpP $
        query >>= \case
            Nothing -> allCmds src
            Just cmd -> cmdHelp src cmd
  where
    allCmds src =
        let cmds = T.unwords . map (T.cons ':') . Map.keys $ m
        in message' src . Message $ "Available Commands: " <> cmds
    cmdHelp src cmd = case Map.lookup cmd m of
        Nothing -> message' src . Message $ "No help found for " <> cmd
        Just ht -> message' src . Message $ cmd <> ": " <> ht
