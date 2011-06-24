{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Aws.Response
where
  
import qualified Data.ByteString         as B
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP
import qualified Network.HTTP.Types      as HTTP

class ResponseIteratee a where
    responseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a
    
instance ResponseIteratee HTTP.Response where
    responseIteratee = HTTP.lbsIter
