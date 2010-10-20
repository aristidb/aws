{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AWS.Response
where
  
import           AWS.Http
import           Control.Applicative
import           Control.Monad
import           Control.Shortcircuit
import           Data.Char
import           Data.Function
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Text.XML.Monad
import qualified Text.XML.Light            as XL
import           MonadLib
import           MonadLib.Compose

data Response
    = Response {
        httpResponse :: HttpResponse
      }
    deriving (Show)

class FromResponse a where
    fromResponse :: Xml ParseError Response a

parseXmlResponse :: Xml ParseError Response XL.Element
parseXmlResponse = parseXMLDoc <<< asks (BLU.toString . responseBody . httpResponse)