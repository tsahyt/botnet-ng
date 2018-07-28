{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Components.Permission
    ( allowed
    , withBlacklist
    , permissions
    , Perm(..)
    , allPerms
    , perm
    , Permissions
    , UserPermissions
    -- * External Update
    , GrantU(..)
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.Acid
import Control.Monad.Reader
import Data.Acid hiding (query, update)
import Data.Map (Map)
import Data.SafeCopy
import Data.Semigroup
import Data.Set (Set)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol)
import Network.Voco.Combinators
import Network.Voco.Core (Bot)
import Network.Yak
import Network.Yak.Client
import Orphans ()

import qualified Data.Attoparsec.Text as A
import qualified Data.Set as Set

data Perm
    = SetPermissions -- ^ "set-perms"
    | ManageChans -- ^ "manage-chans"
    | ChangeNick -- ^ "change-nick"
    | Speak -- ^ "speak"
    deriving (Eq, Ord, Show, Read, Enum)

allPerms :: [Perm]
allPerms = [SetPermissions ..]

permDict :: [(Perm, Text)]
permDict =
    [ (SetPermissions, "set-perms")
    , (ManageChans, "manage-chans")
    , (ChangeNick, "change-nick")
    , (Speak, "speak")
    ]

perm :: A.Parser Perm
perm = A.choice . map (\(p, s) -> p <$ A.string s) $ permDict

newtype Permissions =
    Perms (Set Perm)
    deriving (Show, Semigroup, Monoid)

data UserPermissions = UPerms
    { _uperms :: Map Host Permissions
    , _blacklist :: Set Host
    } deriving (Show)

makeLenses ''UserPermissions

instance Semigroup UserPermissions where
    UPerms m b <> UPerms n c = UPerms (m <> n) (b <> c)

instance Monoid UserPermissions where
    mappend = (<>)
    mempty = UPerms mempty mempty

deriveSafeCopy 0 'base ''Perm

deriveSafeCopy 0 'base ''Permissions

deriveSafeCopy 0 'base ''UserPermissions

allows :: Permissions -> Perm -> Bool
allows (Perms ps) p = p `Set.member` ps

grant :: Permissions -> Perm -> Permissions
grant (Perms ps) p = (Perms $ Set.insert p ps)

revoke :: Permissions -> Perm -> Permissions
revoke (Perms ps) p = (Perms $ Set.delete p ps)

grantU :: Host -> Perm -> Update UserPermissions ()
grantU h p = do
    ps <- use $ uperms . at h
    case ps of
        Nothing -> uperms . at h .= Just (Perms $ Set.singleton p)
        Just _ -> uperms . at h . _Just %= flip grant p

revokeU :: Host -> Perm -> Update UserPermissions ()
revokeU h p = uperms . at h . _Just %= flip revoke p

userPerms :: Host -> Query UserPermissions (Maybe Permissions)
userPerms h = view $ uperms . at h

isBlacklisted :: Host -> Query UserPermissions Bool
isBlacklisted h = do
    b <- view blacklist
    pure $ h `Set.member` b

blacklistHost :: Host -> Update UserPermissions ()
blacklistHost h = blacklist %= Set.insert h

unBlacklistHost :: Host -> Update UserPermissions ()
unBlacklistHost h = blacklist %= Set.delete h

makeAcidic
    ''UserPermissions
    [ 'grantU
    , 'revokeU
    , 'userPerms
    , 'isBlacklisted
    , 'blacklistHost
    , 'unBlacklistHost
    ]

-- | Applies a blacklist contained in the 'UserPermissions's carried in the acid
-- monad underlying a bot.
withBlacklist ::
       (AcidMember UserPermissions s, MonadAcid s m, KnownSymbol c)
    => Bot m (Msg c p) o
    -> Bot m (Msg c p) o
withBlacklist b = do
    i <- msgPrefix <$> query
    case i of
        Just (PrefixUser h) -> do
            bl <- queryAcid $ IsBlacklisted h
            guard (not bl)
            b
        _ -> b

-- | Guard a bot over an IRC message using with a collection of required
-- permissions. The bot will fail as long as any permission in the given
-- 'Foldable' is not present for the user triggering it, as determined by the
-- message prefix.
allowed ::
       (AcidMember UserPermissions s, MonadAcid s m, Foldable t, KnownSymbol c)
    => t Perm
    -> Bot m (Msg c p) o
    -> Bot m (Msg c p) o
allowed ps b = do
    i <- msgPrefix <$> query
    case i of
        Just (PrefixUser h) -> do
            ups <- queryAcid $ UserPerms h
            case ups of
                Just hps -> do
                    guard (all (hps `allows`) ps)
                    b
                _ -> empty
        _ -> empty

data PermCmd
    = Grant Host
            Perm
    | Revoke Host
             Perm
    | Blacklist Host
    | UnBlacklist Host
    deriving (Eq, Show, Ord, Read)

permCmd :: A.Parser PermCmd
permCmd =
    A.choice
        [ Grant <$> (A.string ":grant" *> A.skipSpace *> host) <*>
          (A.skipSpace *> perm)
        , Revoke <$> (A.string ":revoke" *> A.skipSpace *> host) <*>
          (A.skipSpace *> perm)
        , Blacklist <$> (A.string ":blacklist" *> A.skipSpace *> host)
        , UnBlacklist <$> (A.string ":unblacklist" *> A.skipSpace *> host)
        ]
  where
    host = do
        n <- A.takeTill (A.inClass " .!@\r\n")
        p <- A.peekChar
        case p of
            Just c
                | c == '.' -> empty
            _ ->
                Host n <$>
                optional (A.char '!' *> A.takeTill (A.inClass " @\r\n")) <*>
                optional (A.char '@' *> A.takeTill (A.inClass " \r\n"))

-- | A bot capable of updating permissions, which can be used by any user with
-- the 'SetPermissions' permission. See the 'Perm' type ('permDict') for
-- legal permissions.
--
-- Syntax:
--
-- > :grant <host> <permission>
-- > :revoke <host> <permission>
-- > :blacklist <host>
-- > :unblacklist <host>
permissions ::
       (AcidMember UserPermissions s, MonadAcid s m, Monad m)
    => Bot m Privmsg ()
permissions =
    allowed [SetPermissions] . on (view privmsgMessage) . parsedMsg permCmd $
    query >>= \case
        Grant h p -> updateAcid $ GrantU h p
        Revoke h p -> updateAcid $ RevokeU h p
        Blacklist h -> updateAcid $ BlacklistHost h
        UnBlacklist h -> updateAcid $ UnBlacklistHost h
