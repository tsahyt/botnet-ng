{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Components.Misc
    ( source
    , help
    , agencies
    , Combos
    , combos
    ) where

import Control.Lens
import Control.Monad.Acid
import Control.Monad.Random
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Maybe
import Control.Applicative.Combinators
import Components.Permission
import Data.Coproduct
import Data.SafeCopy
import Data.Acid hiding (query, update)
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Network.Voco
import Network.Yak.Client
import Network.Yak.Types
import Text.Printf
import Data.Map (Map)

import qualified Data.Attoparsec.Text as A

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
        , ( ":gnome"
          , [ "Your disgust over KDE has been noted. (Cult Follower #%03d)"
            , "You are now subscribed to GNOME Facts! (Cult Follower #%03d)" ])
        ]
  where
    go src fmt = do
        f <- uniform fmt
        c <- getRandomR (1337 :: Int, 99999 :: Int)
        message' src . Message . pack . printf f $ c

combo :: (MonadIO m, Monad m) => [Text] -> (Channel -> Req ()) -> Bot m Privmsg ()
combo [] _ = pure ()
combo (start:finisher) c =
    answeringP $ \src ->
        case src of
            Left chan ->
                filterB (== Message start) . asyncV . request $ do
                    x <- runMaybeT $ matchReq chan finisher
                    case x of
                        Nothing -> pure ()
                        Just _ -> c chan
            Right _ -> pure ()

matchReq :: Channel -> [Text] -> MaybeT Req ()
matchReq _ [] = pure ()
matchReq src (x:xs) = do
    y <- lift recv
    if Left src `elem` y ^. privmsgTargets
        then guard (y ^. privmsgMessage . _Wrapped == x) *> matchReq src xs
        else matchReq src (x:xs)

data Combo = Combo 
    { comboSequence :: [Text]
    , comboNick :: Nickname
    , comboMsg :: Text }

deriveSafeCopy 0 'base ''Combo

newtype Combos = Combos (Map Nickname Combo)
    deriving (Monoid)

makeWrapped ''Combos

deriveSafeCopy 0 'base ''Combos

delCombo :: Nickname -> Update Combos ()
delCombo n = _Wrapped . at n .= Nothing

addCombo :: Combo -> Update Combos ()
addCombo c = _Wrapped . at (comboNick c) .= Just c

allCombos :: Query Combos [Combo]
allCombos = toListOf (_Wrapped . traverse) <$> ask

makeAcidic
    ''Combos
    [ 'delCombo
    , 'addCombo
    , 'allCombos ]

data ComboCmd
    = DelComboCmd Nickname
    | AddComboCmd Nickname
                  Text
                  [Text]

comboCmd :: A.Parser ComboCmd
comboCmd = A.choice
    [ DelComboCmd <$> (A.string ":del-combo" *> A.skipSpace *> nickP)
    , AddComboCmd <$> (A.string ":add-combo" *> A.skipSpace *> nickP) <*> (A.skipSpace *> msg) <*> (A.skipSpace *> sqs) ]
    where nickP = A.takeWhile (/= ' ')
          msg = between (A.char '"') (A.char '"') (A.takeWhile (/= '"'))
          sqs = A.sepBy1 (A.takeWhile (/= ',')) (A.char ',')

combos ::
       ( AcidMember UserPermissions s
       , AcidMember Combos s
       , MonadAcid s m
       , MonadIO m
       )
    => Bot m Privmsg ()
combos = run <|> mgmt
  where
    run = do
        cs <- queryAcid AllCombos
        asum . map mkCombo $ cs
    mgmt =
        allowed [ManageCombos] . on (view privmsgMessage) . parsedMsg comboCmd $
        query >>= \case
            DelComboCmd n -> updateAcid $ DelCombo n
            AddComboCmd n msg sq -> updateAcid $ AddCombo (Combo sq n msg)
    mkCombo cmb =
        combo (comboSequence cmb) $ \chan ->
            kick chan (comboNick cmb) (Just . Message $ comboMsg cmb)
