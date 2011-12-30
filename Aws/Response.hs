{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}

module Aws.Response
where

import           Data.IORef
import           Data.Monoid
import           Data.Attempt            (Attempt(..))
import qualified Control.Exception       as E
import qualified Control.Failure         as F
import qualified Network.HTTP.Conduit    as HTTP

data Response m a = Response m (Attempt a)
    deriving (Show, Functor)

tellMetadata :: m -> Response m ()
tellMetadata m = Response m (return ())

instance Monoid m => Monad (Response m) where
    return x = Response mempty (Success x)
    Response m1 (Failure e) >>= _ = Response m1 (Failure e)
    Response m1 (Success x) >>= f = let Response m2 y = f x
                                    in Response (m1 `mappend` m2) y -- currently using First-semantics, Last SHOULD work too

instance (Monoid m, E.Exception e) => F.Failure e (Response m) where
    failure e = Response mempty (F.failure e)

tellMetadataRef :: Monoid m => IORef m -> m -> IO ()
tellMetadataRef r m = modifyIORef r (`mappend` m)

class ResponseConsumer r a where
    type ResponseMetadata a
    responseConsumer :: r -> IORef (ResponseMetadata a) -> HTTP.ResponseConsumer IO a

instance ResponseConsumer r HTTP.Response where
    type ResponseMetadata HTTP.Response = ()
    responseConsumer _ _ = HTTP.lbsConsumer
