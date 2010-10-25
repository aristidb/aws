{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Aws.Response
where
  
import           Aws.Http
import           Control.Applicative
import           Control.Monad
import           Control.Shortcircuit
import           Data.Char
import           Data.Function
import           MonadLib
import           MonadLib.Compose
import           Text.XML.Monad
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Text.XML.Light            as XL

data Response
    = Response {
        httpResponse :: HttpResponse
      }
    deriving (Show)

class FromXmlError e => FromResponse a e | a -> e where
    fromResponse :: Xml e Response a

instance FromResponse Response XmlError where
    fromResponse = ask

parseXmlResponse :: FromXmlError e => Xml e Response XL.Element
parseXmlResponse = parseXMLDoc <<< asks (BLU.toString . responseBody . httpResponse)
