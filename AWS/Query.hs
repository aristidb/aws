{-# LANGUAGE RecordWildCards #-}

module AWS.Query
where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Time
import qualified Network.HTTP               as HTTP
import           Network.URI                hiding (query)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.HMAC
import qualified Codec.Binary.Base64        as Base64
import qualified Codec.Binary.UTF8.String   as Utf8
import           System.Locale
  
data API
    = SimpleDB
    deriving (Show)
  
data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

data Query 
    = Query {
        api :: API
      , method :: HTTP.RequestMethod
      , protocol :: Protocol
      , host :: String
      , port :: Int
      , path :: String
      , query :: [(String, String)]
      , date :: Maybe UTCTime
      , metadata :: [HTTP.Header]
      , body :: L.ByteString  
      }
    deriving (Show)

data Credentials
    = Credentials {
        accessKeyID :: String
      , secretAccessKey :: String
      }
    deriving (Show)
      
data TimeInfo
    = Timestamp { fromTimestamp :: UTCTime }
    | Expires { fromExpires :: UTCTime }
    deriving (Show)
      
addTimeInfo :: TimeInfo -> Query -> Query
addTimeInfo (Timestamp time) q@Query{..} = q {
                                             query = ("Timestamp", fmtAmzTime time) : query
                                           , date = Just time
                                           }
addTimeInfo (Expires time) q@Query{..} = q {
                                           query = ("Expires", fmtAmzTime time) : query
                                         }

addSignatureData :: Credentials -> Query -> Query
addSignatureData Credentials{..} q@Query{..} = q {
                                           query = [("AWSAccessKeyId", accessKeyID), ("SignatureMethod", "HmacSHA1"), ("SignatureVersion", "2")]
                                                   ++ query
                                         }
                                               
stringToSign Query{..} = case api of 
                           SimpleDB -> show method ++ "\n" ++
                                       host ++ "\n" ++
                                       path ++ "\n" ++
                                       HTTP.urlEncodeVars sortedQuery
    where sortedQuery = sortBy (compare `on` Utf8.encode . fst) query
                                               
signPreparedQuery :: Credentials -> Query -> Query
signPreparedQuery Credentials{..} q@Query{..} = q { query = ("Signature", sig) : query }
    where sig = Base64.encode $ hmac_sha1 (Utf8.encode secretAccessKey) (Utf8.encode . stringToSign $ q)
                           
signQuery :: TimeInfo -> Credentials -> Query -> Query
signQuery ti cr = signPreparedQuery cr . addSignatureData cr . addTimeInfo ti

queryToHTTPRequest :: Query -> (Protocol, HTTP.Request L.ByteString)
queryToHTTPRequest Query{..}
    = (protocol, req)
    where req = HTTP.Request {
                  rqURI = nullURI {
                            uriPath  = path
                          , uriQuery = '?' : HTTP.urlEncodeVars query
                          }
                , rqMethod = method
                , rqHeaders = (HTTP.Header HTTP.HdrHost host :)
                              . (HTTP.Header HTTP.HdrContentLength (show $ L.length body) :)
                              . addDate
                              $ metadata
                , rqBody = body
                }
          addDate = case date of
                      Nothing -> id
                      Just time -> (HTTP.Header HTTP.HdrDate (fmtRfc822Time time) :)
                      
queryToURI :: Query -> Maybe URI
queryToURI Query{..}
    = case test of
        False -> Nothing
        True -> Just $ nullURI {
                  uriScheme = case protocol of
                                HTTP -> "http:"
                                HTTPS -> "https:"
                , uriAuthority = Just (URIAuth {
                                         uriUserInfo = ""
                                       , uriRegName = host
                                       , uriPort = ':' : show port
                                       })
                , uriPath = path
                , uriQuery = '?' : HTTP.urlEncodeVars query
                }
      where test = (L.null body) && (null metadata)

fmtTime :: String -> UTCTime -> String
fmtTime = formatTime defaultTimeLocale

fmtRfc822Time :: UTCTime -> String
fmtRfc822Time = fmtTime rfc822DateFormat

fmtAmzTime :: UTCTime -> String
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"
