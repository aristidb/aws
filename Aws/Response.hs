{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable #-}

module Aws.Response
where
  
import           Control.Monad.Compose.Class
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Text.XML.Monad
import qualified Control.Failure             as F
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy.UTF8   as BLU
import qualified Data.Enumerator             as En
import qualified Network.HTTP.Enumerator     as HTTP
import qualified Network.Wai                 as Wai
import qualified Text.XML.Light              as XL

class ResponseIteratee a where
    responseIteratee :: Wai.Status -> HTTP.Headers -> En.Iteratee B.ByteString IO a
    
instance ResponseIteratee HTTP.Response where
    responseIteratee = HTTP.lbsIter

xmlResponseIteratee :: (Monad m, F.Failure e m) => Xml e HTTP.Response a -> Wai.Status -> HTTP.Headers -> En.Iteratee B.ByteString m a
xmlResponseIteratee xml status headers = do
  body <- HTTP.lbsIter status headers
  case runXml xml body of
    Left e -> En.Iteratee $ F.failure e
    Right a -> return a

parseXmlResponse :: (FromXmlError e, Error e) => Xml e HTTP.Response XL.Element
parseXmlResponse = parseXMLDoc <<< asks (BLU.toString . HTTP.responseBody)
