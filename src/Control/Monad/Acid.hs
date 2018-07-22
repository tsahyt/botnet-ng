{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Acid
    ( AcidStates(..)
    , MonadAcid(..)
    , AcidT
    , runAcidT
    , mapAcidT
    , AcidMember(..)
    ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Zip
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.List
import Control.Monad.Trans.RWS (RWST)
import Network.Voco.Core (Bot, liftBot)
import Data.Acid hiding (update, query)
import Data.Acid.Advanced

data AcidStates (ts :: [*]) where
    NullState :: AcidStates '[]
    (:+) :: AcidState t -> AcidStates ts -> AcidStates (t ': ts)

class AcidMember x (xs :: [*]) where
    member :: AcidStates xs -> AcidState x

instance AcidMember x rs => AcidMember x (a ': rs) where
    member (_ :+ rs) = member rs

instance AcidMember x (x ': xs) where
    member (x :+ _) = x

newtype AcidT (ts :: [*]) m a = AcidT
    { runAcidT' :: ReaderT (AcidStates ts) m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadFix
               , MonadFail
               , MonadTrans
               , MonadZip
               , Alternative
               , MonadPlus
               , MonadCont
               )

runAcidT :: AcidT ts m a -> AcidStates ts -> m a
runAcidT k s = runReaderT (runAcidT' k) s

mapAcidT :: (m a -> n b) -> AcidT s m a -> AcidT s n b
mapAcidT f (AcidT k) = AcidT (mapReaderT f k)

instance MonadError e m => MonadError e (AcidT s m) where
    throwError = lift . throwError
    catchError f m = AcidT $ catchError (runAcidT' f) (\e -> runAcidT' (m e))

instance MonadState s m => MonadState s (AcidT st m) where
    state = lift . state

instance MonadReader r m => MonadReader r (AcidT s m) where
    reader = lift . reader
    local = mapAcidT . local

instance MonadWriter r m => MonadWriter r (AcidT s m) where
    writer = lift . writer
    tell = lift . tell
    listen k = AcidT $ listen (runAcidT' k)
    pass k = AcidT $ pass (runAcidT' k)

class MonadIO m =>
      MonadAcid ts m | m -> ts where
    updateAcid ::
           (AcidMember s ts, s ~ MethodState e, UpdateEvent e)
        => e
        -> m (MethodResult e)
    queryAcid ::
           (AcidMember s ts, s ~ MethodState e, QueryEvent e)
        => e
        -> m (MethodResult e)

instance MonadIO m => MonadAcid ts (AcidT ts m) where
    updateAcid u = AcidT $ do
        st <- member <$> ask
        update' st u
    queryAcid u = AcidT $ do
        st <- member <$> ask
        query' st u

instance MonadAcid s m => MonadAcid s (StateT st m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (ReaderT r m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (ExceptT r m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (IdentityT m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (MaybeT m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance (Monoid w, MonadAcid s m) => MonadAcid s (WriterT w m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (ContT w m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance (Monoid w, MonadAcid s m) => MonadAcid s (RWST r w st m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (ListT m) where
    updateAcid = lift . updateAcid
    queryAcid = lift . queryAcid

instance MonadAcid s m => MonadAcid s (Bot m i) where
    updateAcid = liftBot . updateAcid
    queryAcid = liftBot . queryAcid
