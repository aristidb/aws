{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Aws.Response
where
  
import           Control.Monad.Compose.Class
import           Control.Monad.Error.Class
import           Control.Monad.Reader.Class
import           Text.XML.Monad
import qualified Data.ByteString.Lazy.UTF8   as BLU
import qualified Network.HTTP.Enumerator     as HTTP
import qualified Text.XML.Light              as XL

class FromResponse a e where
    fromResponse :: HTTP.Response -> Either e a

instance FromResponse HTTP.Response e where
    fromResponse = Right

fromResponseXml :: Xml e HTTP.Response a -> HTTP.Response -> Either e a
fromResponseXml = runXml

parseXmlResponse :: (FromXmlError e, Error e) => Xml e HTTP.Response XL.Element
parseXmlResponse = parseXMLDoc <<< asks (BLU.toString . HTTP.responseBody)
