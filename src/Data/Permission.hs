{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Permission
    ( allowed
    , Perm(..)
    , Permissions
    , UserPermissions
    -- * Pure Permission Changes
    , allows
    , grant
    , revoke
    -- * Acid Queries/Updates
    , GrantU(..)
    , RevokeU(..)
    , UserPerms(..)
    ) where

import Control.Lens
import Control.Monad.Acid
import Control.Monad.Reader
import Data.Acid hiding (query, update)
import Data.Map (Map)
import Data.SafeCopy
import Data.Semigroup
import Data.Set (Set)
import Network.Voco.Combinators
import Network.Voco.Core (Bot, liftBot)
import Network.Yak

import qualified Data.Set as Set

data Perm =
    Perm
    deriving (Eq, Ord, Show, Read)

newtype Permissions =
    Perms (Set Perm)
    deriving (Semigroup, Monoid)

newtype UserPermissions =
    UPerms (Map Host Permissions)
    deriving (Semigroup, Monoid)

makeWrapped ''UserPermissions
deriveSafeCopy 0 'base ''Perm
deriveSafeCopy 0 'base ''Permissions
deriveSafeCopy 0 'base ''Host
deriveSafeCopy 0 'base ''UserPermissions

allows :: Permissions -> Perm -> Bool
allows (Perms ps) p = p `Set.member` ps

grant :: Permissions -> Perm -> Permissions
grant (Perms ps) p = (Perms $ Set.insert p ps)

revoke :: Permissions -> Perm -> Permissions
revoke (Perms ps) p = (Perms $ Set.delete p ps)

grantU :: Host -> Perm -> Update UserPermissions ()
grantU h p = _Wrapped . at h . _Just %= flip grant p

revokeU :: Host -> Perm -> Update UserPermissions ()
revokeU h p = _Wrapped . at h . _Just %= flip revoke p

userPerms :: Host -> Query UserPermissions (Maybe Permissions)
userPerms h = view $ _Wrapped . at h

makeAcidic ''UserPermissions ['grantU, 'revokeU, 'userPerms]

-- | Guard a bot over an IRC message using with a collection of required
-- permissions. The bot will fail as long as any permission in the given
-- 'Foldable' is not present for the user triggering it, as determined by the
-- message prefix.
allowed ::
       (AcidMember UserPermissions s, MonadAcid s m, Foldable t)
    => t Perm
    -> Bot m (Msg c p) o
    -> Bot m (Msg c p) o
allowed ps b = do
    i <- msgPrefix <$> query
    case i of
        Just (PrefixUser h) -> do
            uperms <- queryAcid $ UserPerms h
            case uperms of
                Just hps -> do
                    guard (all (hps `allows`) ps)
                    b
                _ -> empty
        _ -> empty
