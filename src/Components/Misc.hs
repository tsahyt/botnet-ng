{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.Misc
    ( source
    , help
    , agencies
    ) where

import Control.Monad.Random
import Data.Coproduct
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Network.Voco
import Network.Yak.Client
import Network.Yak.Types
import Text.Printf

url :: Text
url = "https://github.com/tsahyt/botnet-ng"

source :: MonadChan m => Bot m (Privmsg :|: Notice) ()
source =
    answering $ \src ->
        filterB (== ":source") $
        message' src $ Message url

help :: MonadChan m => Bot m (Privmsg :|: Notice) ()
help =
    answering $ \src ->
        filterB (== ":help") $
        message' src . Message $ url <> "/tree/master/doc/help.md"

agencies :: (MonadRandom m, MonadChan m) => Bot m Privmsg ()
agencies =
    answeringP $ \src ->
        asum . map (\(cmd, fmt) -> filterB (== cmd) (go src fmt)) $
        [ (":cia"
          , ["This incident has been reported (Case #%05d)"
            ,"This regime will be overthrown (Useless War #%05d)"])
        , ( ":fbi"
          , [ "This collusion with Russia has been filed and will be used\
           \ against you at a politically opportune moment (Case #%05d)"
            ])
        , ( ":fiveeyes"
          , [ "This communication has been intercepted and will be shared among\
           \ members of the Five Eyes (Case #%05d)"
            ])
        , ( ":kgb"
          , [ "The Party has been notified of your bourgeois thought.\
           \ (Gulag Inmate #%05d)"
            ])
        , ( ":eu"
          , [ "Your attempt at executing a mutually beneficial trade has been\
             \ brought before the European Commission. (Anti-Trust Case #%05d)"
            , "Your speech has been regulated. (EU Law #%05d)"
            ])
        , ( ":gnu"
          , [ "Your use of proprietary software has been reported to the\
             \ FSF. (Assigned GNULAG #%03d)" ] )
        ]
  where
    go src fmt = do
        f <- uniform fmt
        c <- getRandomR (1337 :: Int, 99999 :: Int)
        message' src . Message . pack . printf f $ c
