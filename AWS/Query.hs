{-# LANGUAGE RecordWildCards #-}

module AWS.Query
where

import           Control.Monad
import           Data.List
import           Data.Time
import qualified Network.HTTP               as HTTP
import           Network.URI                hiding (query)
import qualified Data.ByteString.Lazy.Char8 as L
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
      , metadata :: [HTTP.Header]
      , body :: L.ByteString  
      }
    deriving (Show)

data Credentials
    = Credentials {
        accessKeyID :: String
      , secretAccessKey :: String
      }

ø :: MonadPlus m => m a
ø = mzero
      
queryToHTTPRequest :: UTCTime -> Credentials -> Query -> (Protocol, HTTP.Request L.ByteString)
queryToHTTPRequest time Credentials{..} Query{..}
    = (protocol, req)
    where req = HTTP.Request {
                  rqURI = URI {
                            uriScheme    = ø
                          , uriAuthority = ø
                          , uriPath      = path
                          , uriQuery     = '?' : HTTP.urlEncodeVars expandedQuery
                          , uriFragment  = ø
                          }
                , rqMethod = method
                , rqHeaders = HTTP.Header HTTP.HdrHost host
                              : HTTP.Header HTTP.HdrContentLength (show $ L.length body)
                              : HTTP.Header HTTP.HdrDate (fmtTime rfc822DateFormat time)
                              : metadata
                , rqBody = body
                }
          expandedQuery = ("Timestamp", fmtTime "%Y-%m-%dT%H:%M:%S" time)
                          : ("AWSAccessKeyId", accessKeyID)
                          : query

fmtTime :: String -> UTCTime -> String
fmtTime = formatTime defaultTimeLocale
