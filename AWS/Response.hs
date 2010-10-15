module AWS.Response
where
  
import           AWS.Http
import           Text.XML.Light
import qualified Data.ByteString.Lazy.UTF8 as BLU

data Response
    = Response {
        httpResponse :: HttpResponse
      }
    deriving (Show)

class FromResponse a where
    fromResponse :: Response -> Maybe a

parseXmlResponse :: HttpResponse -> Maybe Element
parseXmlResponse = parseXMLDoc . BLU.toString . responseBody
