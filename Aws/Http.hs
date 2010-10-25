{-# LANGUAGE RecordWildCards #-}

module Aws.Http
where
  
import           Aws.Util
import           Control.Applicative
import           Control.Monad
import           Data.Time
import           Data.Time.Clock.POSIX
import           Network.Curl
import           Network.URI
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Network.HTTP               as HTTP
  
data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443

-- Note/TODO: Large data: just use files
  
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
        responseError :: Maybe HttpError
      , responseStatus :: Int
      , responseBody :: L.ByteString
      }
    deriving (Show)

data HttpError
    = CurlError CurlCode
    | OtherError String
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
                    [
                      CurlHttpHeaders headers
                    , CurlFailOnError False
                    ]
                    ++ otherOptions
          headers = case requestDate of
                      Just d -> ["Date: " ++ fmtRfc822Time d]
                      Nothing -> []
          parse :: CurlResponse_ [(String, String)] L.ByteString -> HttpResponse
          parse CurlResponse{..} = HttpResponse {
                                          responseError = CurlError respCurlCode <$ guard (respCurlCode /= CurlOK)
                                        , responseStatus = respStatus
                                        , responseBody = respBody
                                        }
