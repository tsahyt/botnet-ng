{-# LANGUAGE OverloadedStrings #-}

module Components.Interject
    ( interject
    ) where

import Control.Lens (_Wrapped, view)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.Voco
import Network.Yak.Client
import Network.Yak.Types (Message(..))

import qualified Data.Attoparsec.Text as A

interject :: MonadChan m => Bot m Privmsg ()
interject =
    answeringP $ \src ->
        on (view _Wrapped) $
        parsed parseInterjection $ do
            i <- query
            let int =
                    case i of
                        Full gnu linux posix -> interjection gnu linux posix
                        Short gnu linux -> shortInterjection gnu linux
                        Empty -> shortInterjection "GNU" "Linux"
            message' src (Message int)

data Interjection
    = Full Text
           Text
           Text
    | Short Text
            Text
    | Empty
    deriving (Show)

parseInterjection :: A.Parser Interjection
parseInterjection =
    A.string ":interject" *> A.skipSpace *>
    (A.try full <|> A.try short <|> pure Empty) <* A.endOfInput
  where
    full = Full <$> component <*> component <*> component
    short = Short <$> component <*> component
    component = A.takeWhile1 (not . isSep) <* A.skipSpace
    isSep c = isSpace c || A.inClass ",;:./" c

interjection :: Text -> Text -> Text -> Text
interjection gnu linux posix =
    shortInterjection gnu linux <>
    mconcat
        [ linux
        , " is not an operating system unto itself, but rather another free\
          \ component of a fully functioning "
        , gnu
        , " system made useful by the "
        , gnu
        , " corelibs, shell utilities, and vital system components comprising\
          \ a full OS as defined by "
        , posix
        ]

shortInterjection :: Text -> Text -> Text
shortInterjection gnu linux =
    mconcat
        [ "I'd just like to interject for a moment. What you're referring to as "
        , linux
        , ", is in fact, "
        , gnuslashlinux
        , " or as I've recently taken to calling it, "
        , gnupluslinux
        , ". "
        ]
  where
    gnuslashlinux = gnu <> "/" <> linux
    gnupluslinux = gnu <> " plus " <> linux
