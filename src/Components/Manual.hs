{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Components.Manual
    ( man
    ) where

import Control.Applicative
import Control.Lens hiding ((<|))
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (foldl')
import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty, (<|), head, toList)
import Data.Map (Map)
import Data.Tuple (swap)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Network.Voco
import Network.Wreq
import Network.Wreq.Types (ResponseChecker)
import Network.Yak.Client
import Network.Yak.Types (Message(..))
import Data.FileEmbed

import qualified Data.Attoparsec.Text as A
import qualified Data.Map as M

import Prelude hiding (head)

data Man =
    Man Text
        (Maybe Int)
    deriving (Show)

mancmd :: A.Parser Man
mancmd = A.string ":man" *> A.skipSpace *> A.choice
    [ (\s t -> Man t s) <$> (Just <$> A.decimal) <*> (A.skipSpace *> mantitle)
    , Man <$> mantitle <*> (A.char '(' *> (Just <$> A.decimal) <* A.char '(')
    , Man <$> mantitle <*> optional (A.skipSpace *> A.decimal) ]
  where
    mantitle = A.takeTill isSpace

baseUrl :: String
baseUrl = "https://linux.die.net/man/"

manEntry :: A.Parser (Int, Text, Text)
manEntry =
    (,,) <$> (A.decimal <* A.char ';') <*> (A.takeTill (== ';') <* A.char ';') <*>
    A.takeTill A.isEndOfLine
    
mkPages :: Text -> Map Text (NonEmpty (Int, Text))
mkPages =
    build .
    fromMaybe [] .
    either (error) Just . A.parseOnly (A.many1 $ manEntry <* A.endOfLine)
  where
    build = foldl' (\m (s, t, d) -> M.alter (go s d) t m) M.empty
    go s d Nothing = Just (pure (s, d))
    go s d (Just xs) = Just ((s, d) <| xs)

pages :: Map Text (NonEmpty (Int, Text))
pages = mkPages $(embedStringFile "etc/mandb")
        
findSection :: Text -> Maybe Int
findSection t = fst . head <$> M.lookup t pages

findDescription :: Text -> Int -> Maybe Text
findDescription t s = do
    ps <- M.lookup t pages
    lookup s (toList ps)

unchecked :: ResponseChecker
unchecked _ _ = pure ()

man :: (MonadChan m, MonadIO m) => Bot m Privmsg ()
man =
    answeringP $ \src ->
        on (view _Wrapped) . parsed mancmd $ do
            Man page section <- query
            x <-
                liftIO . runMaybeT $ do
                    section' <- MaybeT . pure $ section <|> findSection page
                    desc <- MaybeT . pure $ findDescription page section'
                    let url = baseUrl <> show section' <> "/" <> unpack page
                    r <-
                        lift $
                        getWith (defaults & checkResponse .~ Just unchecked) url
                    guard (r ^. responseStatus . statusCode == 200)
                    pure (pack url, desc)
            case x of
                Nothing -> message' src "Could not find requested manpage!"
                Just (url, desc) ->
                    message' src . Message $ url <> " - " <> page <> ": " <>
                    desc
