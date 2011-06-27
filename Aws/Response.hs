{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, DeriveFunctor, TypeFamilies #-}

module Aws.Response
where
  
import           Data.IORef
import           Data.Monoid
import qualified Control.Exception       as E
import qualified Control.Failure         as F
import qualified Data.ByteString         as B
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP
import qualified Network.HTTP.Types      as HTTP
  
class WithMetadata a where
    putMetadata :: m -> a () -> a m

data Response m a = Response m (Either E.SomeException a)
    deriving (Functor)

tellMetadata :: m -> Response m ()
tellMetadata m = Response m (return ())

instance Monoid m => Monad (Response m) where
    return x = Response mempty (Right x)
    Response m1 (Left e)  >>= _ = Response m1 (Left e)
    Response m1 (Right x) >>= f = let Response m2 y = f x
                                  in Response (m1 `mappend` m2) y -- currently using First-semantics, Last SHOULD work too

instance (Monoid m, E.Exception e) => F.Failure e (Response m) where
    failure e = Response mempty (Left $ E.toException e)

class ResponseIteratee a where
    type ResponseMetadata a
    responseIteratee :: IORef (ResponseMetadata a) -> HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
    
instance ResponseIteratee HTTP.Response where
    type ResponseMetadata HTTP.Response = ()
    responseIteratee _ = HTTP.lbsIter
