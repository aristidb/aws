module AWS.Response
where
  
import           AWS.Http
import           Text.XML.Light
import qualified Data.ByteString.Lazy.UTF8 as BLU

parseXmlResponse :: HttpResponse -> Maybe Element
parseXmlResponse = parseXMLDoc . BLU.toString . responseBody