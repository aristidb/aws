{-# LANGUAGE RecordWildCards #-}

module AWS.Http
where
  
import           Control.Applicative
import           Control.Monad
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Curl
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
        requestMethod :: HTTP.RequestMethod
      , requestDate :: Maybe UTCTime
      , requestUri :: URI
      , requestPostQuery :: [String]
      , requestBody :: L.ByteString
      }
    deriving (Show)

data HttpResponse
    = HttpResponse {
        responseError :: Maybe String
      , responseStatus :: Maybe Int
      , responseBody :: L.ByteString
      }
    deriving (Show)
             
curlRequest :: [CurlOption] -> HttpRequest -> IO HttpResponse
curlRequest otherOptions HttpRequest{..} = parse <$> curlGetResponse_ uriString options
    where uriString = show requestUri
          options = (case requestMethod of
                      HTTP.GET -> [CurlHttpGet True]
                      HTTP.POST -> [CurlPostFields requestPostQuery]) ++
                    (case requestDate of
                       Just d -> [CurlTimeValue . round . utcTimeToPOSIXSeconds $ d]
                       Nothing -> []) ++
                    [CurlFailOnError False]
                    ++ otherOptions
          parse :: CurlResponse_ [(String, String)] L.ByteString -> HttpResponse
          parse CurlResponse{..} = HttpResponse {
                                          responseError = show respCurlCode <$ guard (respCurlCode /= CurlOK)
                                        , responseStatus = respStatus <$ guard (respStatus /= 0)
                                        , responseBody = respBody
                                        }
