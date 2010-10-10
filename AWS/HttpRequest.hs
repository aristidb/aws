module AWS.HttpRequest
where
  
import qualified Network.HTTP               as HTTP
import           Network.URI
import qualified Data.ByteString.Lazy.Char8 as L
  
data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443
  
data HttpRequest
    = HttpRequest {
        method :: HTTP.RequestMethod
      , uri :: URI
      , postQuery :: [String]
      , body :: L.ByteString
      }
    deriving (Show)
