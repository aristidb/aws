{-# LANGUAGE DeriveDataTypeable #-}

module AttemptT 
( AttemptT(..)
, mapAttemptT
) where

import Data.Typeable
import Data.Attempt

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad

import Control.Exception

-- -------------------------------------------------------------------------- --
-- AttemptT

newtype AttemptT m a = AttemptT { runAttemptT :: m (Attempt a) }

mapAttemptT :: (m (Attempt a) -> n (Attempt b)) -> AttemptT m a -> AttemptT n b
mapAttemptT f = AttemptT . f . runAttemptT

instance (Functor m) => Functor (AttemptT m) where
    fmap f = mapAttemptT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (AttemptT m) where
    pure = return
    (<*>) = ap

instance (Functor m, Monad m) => Alternative (AttemptT m) where
     empty = mzero
     (<|>) = mplus

data AttemptException = FailException String
                      | EmptyException
    deriving (Show, Typeable)

instance Exception AttemptException

instance (Monad m) => Monad (AttemptT m) where
    fail e = AttemptT $ return (Failure (FailException e))
    return a = AttemptT $ return (Success a)
    m >>= k = AttemptT $ do
        a <- runAttemptT m
        case a of 
            Failure e -> return (Failure e)
            Success s -> runAttemptT (k s)

instance (Monad m) => MonadPlus (AttemptT m) where
    mzero = AttemptT $ return (Failure EmptyException)
    m `mplus` n = AttemptT $ do
        a <- runAttemptT m
        case a of
            Failure _ -> runAttemptT n
            Success s -> return (Success s)

instance MonadTrans AttemptT where
    lift m = AttemptT $ do
        a <- m
        return (Success a)

instance (MonadIO m) => MonadIO (AttemptT m) where
    liftIO = lift . liftIO

