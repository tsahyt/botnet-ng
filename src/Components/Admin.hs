{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.Admin
    ( admin
    ) where

import Components.Permission
import Control.Lens (_Wrapped, view)
import Control.Monad.Acid
import Data.Text (cons)
import Network.Voco hiding (join)
import Network.Yak (Channel(..), Message(..))
import Network.Yak.Client

import qualified Data.Attoparsec.Text as A
import qualified Network.Voco.Transmit as T

admin ::
       (AcidMember UserPermissions s, MonadAcid s m, MonadChan m)
    => Bot m Privmsg ()
admin =
    allowed
        [ManageChans]
        (on (view $ privmsgMessage . _Wrapped) $ join <|> leave) <|>
    allowed 
        [ChangeNick] 
        (on (view $ privmsgMessage . _Wrapped) chNick) <|>
    allowed 
        [Speak] 
        (on (view $ privmsgMessage . _Wrapped) say)
  where
    join =
        parsed (A.string ":join" *> A.skipSpace *> channelP) $ 
        query >>= T.join'
    leave =
        parsed (A.string ":leave" *> A.skipSpace *> channelP) $
        query >>= flip T.part' Nothing
    chNick =
        parsed (A.string ":nick" *> A.skipSpace *> A.takeText) $
        query >>= T.nick
    say =
        parsed (A.string ":say" *> A.skipSpace *> sayP) $
        query >>= uncurry message

channelP :: A.Parser Channel
channelP = do
    mark <- A.satisfy (A.inClass "&#")
    name <- A.takeWhile (A.notInClass " \7,\n")
    pure . Channel $ mark `cons` name

sayP :: A.Parser (Channel, Message)
sayP = (,) <$> channelP <*> (Message <$> (A.skipSpace *> A.takeText))
