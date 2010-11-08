{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings #-}

module Aws.Query
where

import           Aws.Http
import           Aws.Util
import           Control.Arrow
import           Control.Monad
import           Data.Time
import           Network.URI                (nullURI, URI(..), URIAuth(..))
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Enumerator    as HTTP

class AsQuery d i | d -> i where
    asQuery :: i -> d -> Query
  
data Api
    = SimpleDB
    deriving (Show)

data Query 
    = Query {
        api :: Api
      , method :: Method
      , protocol :: Protocol
      , host :: String
      , port :: Int
      , path :: String
      , query :: [(String, String)]
      , date :: Maybe UTCTime
      , body :: L.ByteString  
      }
    deriving (Show)

instance AsQuery Query () where
    asQuery _ = id
             
addQuery :: [(String, String)] -> Query -> Query
addQuery xs q = q { query = xs ++ query q }

addQueryIf :: Bool -> [(String, String)] -> Query -> Query
addQueryIf True  = addQuery
addQueryIf False = const id

addQueryUnless :: Bool -> [(String, String)] -> Query -> Query
addQueryUnless = addQueryIf . not
      
addQueryMaybe :: (a -> String) -> (String, Maybe a) -> Query -> Query
addQueryMaybe f (name, Just a) q = q { query = (name, f a) : query q }
addQueryMaybe _ (_, Nothing) q = q

dot :: String -> String -> String
dot x y = x ++ '.' : y

queryList :: (a -> [(String, String)]) -> String -> [a] -> [(String, String)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . show) [(1 :: Int) ..]
          combine pf = map $ first (pf `dot`)
          
addQueryList :: (a -> [(String, String)]) -> String -> [a] -> Query -> Query 
addQueryList f prefix xs = addQuery $ queryList f prefix xs
          
awsBool :: Bool -> String
awsBool True = "true"
awsBool False = "false"

awsTrue :: String
awsTrue = awsBool True

awsFalse :: String
awsFalse = awsBool False

queryToRequest :: Query -> HttpRequest
queryToRequest Query{..}
    = HttpRequest { 
        requestMethod = method
      , requestUri = nullURI {
                       uriScheme = case protocol of
                                     HTTP -> "http:"
                                     HTTPS -> "https:"
                     , uriAuthority = Just URIAuth {
                                        uriUserInfo = ""
                                      , uriRegName = host
                                      , uriPort = if port == defaultPort protocol then "" else ':' : show port
                                      }
                     , uriPath = path
                     , uriQuery = guard isGet >> ('?' : urlEncodeVars query)
                     }
      , requestDate = date
      , requestPostQuery = guard isPost >> map urlEncodeSingleVar query
      , requestBody = L.empty
      }
    where isGet = method == GET
          isPost = method == POST
          urlEncodeSingleVar (a, b) = urlEncode a ++ '=' : urlEncode b

queryToHttpRequest :: Query -> HTTP.Request
queryToHttpRequest Query{..}
    = HTTP.Request {
        HTTP.method = case method of
                        GET -> "GET"
                        POST -> "POST"
      , HTTP.secure = case protocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.host = B8.pack host -- TODO: use ByteString in Query too
      , HTTP.port = port
      , HTTP.path = B8.pack path -- TODO: use ByteString
      , HTTP.queryString = case method of
                             GET -> map (B8.pack *** B8.pack) query
                             POST -> []
      , HTTP.requestHeaders = [("Date", B8.pack $ fmtRfc822Time d) | Just d <- [date]]
                              ++ [("Content-Type", "application/x-www-form-urlencoded") | method == POST]
      , HTTP.requestBody = case method of
                             GET -> L.empty
                             POST -> L8.pack $ urlEncodeVars query
      }

queryToUri :: Query -> URI
queryToUri Query{..} 
    = nullURI {
        uriScheme = case protocol of
                      HTTP -> "http:"
                      HTTPS -> "https:"
      , uriAuthority = Just URIAuth {
                         uriUserInfo = ""
                       , uriRegName = host
                       , uriPort = if port == defaultPort protocol then "" else ':' : show port
                       }
      , uriPath = path
      , uriQuery = '?' : urlEncodeVars query
      }
