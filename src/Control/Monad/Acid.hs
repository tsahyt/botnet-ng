{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Acid
    ( MonadAcid(..)
    , AcidT
    , runAcidT
    , mapAcidT
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
import Data.Acid hiding (update, query)
import Data.Acid.Advanced

newtype AcidT s m a = AcidT
    { runAcidT' :: ReaderT (AcidState s) m a
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

runAcidT :: AcidT s m a -> AcidState s -> m a
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

class MonadIO m => MonadAcid s m | m -> s where
    update :: (s ~ MethodState e, UpdateEvent e) => e -> m (MethodResult e)
    query :: (s ~ MethodState e, QueryEvent e) => e -> m (MethodResult e)

instance MonadIO m => MonadAcid s (AcidT s m) where
    update u = AcidT $ do
        st <- ask
        update' st u
    query u = AcidT $ do
        st <- ask
        query' st u

instance MonadAcid s m => MonadAcid s (StateT st m) where
    update = lift . update
    query = lift . query

instance MonadAcid s m => MonadAcid s (ReaderT r m) where
    update = lift . update
    query = lift . query

instance MonadAcid s m => MonadAcid s (ExceptT r m) where
    update = lift . update
    query = lift . query

instance MonadAcid s m => MonadAcid s (IdentityT m) where
    update = lift . update
    query = lift . query

instance MonadAcid s m => MonadAcid s (MaybeT m) where
    update = lift . update
    query = lift . query

instance (Monoid w, MonadAcid s m) => MonadAcid s (WriterT w m) where
    update = lift . update
    query = lift . query

instance MonadAcid s m => MonadAcid s (ContT w m) where
    update = lift . update
    query = lift . query

instance (Monoid w, MonadAcid s m) => MonadAcid s (RWST r w s m) where
    update = lift . update
    query = lift . query

instance MonadAcid s m => MonadAcid s (ListT m) where
    update = lift . update
    query = lift . query
