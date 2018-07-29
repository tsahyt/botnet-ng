{-# LANGUAGE OverloadedStrings #-}

module Components.Interject
    ( interject
    ) where

import Control.Lens (_Wrapped, view)
import Control.Monad.Combinators
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
                        Full gnu linux posix -> mkInterject gnu linux posix
                        Short gnu linux -> mkInterjectS gnu linux
                        Empty -> mkInterjectS "GNU" "Linux"
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
    A.string ":interject" *> A.skipSpace *> (full <|> short <|> pure Empty) <*
    A.endOfInput
  where
    full = Full <$> (component <* sep) <*> (component <* sep) <*> component
    short = Short <$> (component <* sep) <*> component
    component = (quoted <|> single)
    quoted = between (A.char '"') (A.char '"') (A.takeWhile1 (/= '"'))
    single = A.takeWhile1 (not . isSep)
    sep = (() <$ A.satisfy isSep) <* A.skipSpace
    isSep = A.inClass ",;:./ "

mkInterject :: Text -> Text -> Text -> Text
mkInterject gnu linux posix =
    mkInterjectS gnu linux <>
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

mkInterjectS :: Text -> Text -> Text
mkInterjectS gnu linux =
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
