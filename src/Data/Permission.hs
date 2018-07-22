{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Permission
    ( Perm(..)
    , Permissions
    , allows
    , grant
    , revoke
    , HasPermissions(..)
    , allowed
    ) where

import Control.Lens.TH
import Data.Semigroup
import Data.Set (Set, delete, insert, member)
import Network.Voco.Core (Bot)
import Network.Voco.Combinators
import Network.Yak

data Perm = Perm
    deriving (Eq, Ord, Show, Read)

newtype Permissions =
    Perms (Set Perm)
    deriving (Semigroup, Monoid)

makeClassy ''Permissions

allows :: Permissions -> Perm -> Bool
allows (Perms ps) p = p `member` ps

grant :: Permissions -> Perm -> Permissions
grant (Perms ps) p = (Perms $ insert p ps)

revoke :: Permissions -> Perm -> Permissions
revoke (Perms ps) p = (Perms $ delete p ps)

allowed :: Foldable t => t Perm -> Bot m (Msg c p) o -> Bot m (Msg c p) o
allowed ps b = undefined
