{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Aws.Response
where
  
import           Data.IORef
import qualified Data.ByteString         as B
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP
import qualified Network.HTTP.Types      as HTTP
  
class Metadata m where
    emptyMetadata :: m

instance Metadata () where
    emptyMetadata = ()

class WithMetadata a where
    putMetadata :: m -> a () -> a m

class ResponseIteratee a where
    type ResponseMetadata a
    responseIteratee :: IORef (ResponseMetadata a) -> HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
    
instance ResponseIteratee HTTP.Response where
    type ResponseMetadata HTTP.Response = ()
    responseIteratee _ = HTTP.lbsIter
