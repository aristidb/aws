{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeFamilies, OverloadedStrings #-}

module Aws.Query
where

import           Aws.Http
import           Aws.Util
import           Control.Arrow
import           Data.Maybe
import           Data.Time
import           Network.URI             (nullURI, URI(..), URIAuth(..))
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import qualified Data.ByteString.UTF8    as BU
import qualified Network.HTTP.Enumerator as HTTP

class AsQuery r where
    type Info r :: *
    asQuery :: Info r -> r -> Query
  
data Api
    = SimpleDB
    | S3
    deriving (Show)

data AuthorizationMethod
    = AuthorizationNone
    | AuthorizationQuery
    | AuthorizationHeader
    deriving (Show)

data AuthorizationHash
    = HmacSHA1
    | HmacSHA256
    deriving (Show)

data Query 
    = Query {
        api :: Api
      , authorizationMethod :: AuthorizationMethod
      , authorizationHash :: AuthorizationHash
      , method :: Method
      , protocol :: Protocol
      , host :: B.ByteString
      , port :: Int
      , path :: B.ByteString
      , canonicalizedResource :: B.ByteString
      , subresource :: Maybe B.ByteString
      , query :: [(B.ByteString, B.ByteString)]
      , date :: Maybe UTCTime
      , authorization :: Maybe B.ByteString
      , contentType :: Maybe B.ByteString
      , contentMd5 :: Maybe B.ByteString
      , body :: L.ByteString
      }
    deriving (Show)

instance AsQuery Query where
    type Info Query = ()
    asQuery _ = id
             
addQuery :: [(B.ByteString, B.ByteString)] -> Query -> Query
addQuery xs q = q { query = xs ++ query q }

addQueryItem :: B.ByteString -> B.ByteString -> Query -> Query
addQueryItem name value = addQuery [(name, value)]

addQueryIf :: Bool -> [(B.ByteString, B.ByteString)] -> Query -> Query
addQueryIf True  = addQuery
addQueryIf False = const id

addQueryUnless :: Bool -> [(B.ByteString, B.ByteString)] -> Query -> Query
addQueryUnless = addQueryIf . not
      
addQueryMaybe :: (a -> B.ByteString) -> (B.ByteString, Maybe a) -> Query -> Query
addQueryMaybe f (name, Just a) q = q { query = (name, f a) : query q }
addQueryMaybe _ (_, Nothing) q = q

dot :: B.ByteString -> B.ByteString -> B.ByteString
dot x y = B.concat [x, BU.fromString ".", y]

queryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> [(B.ByteString, B.ByteString)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . BU.fromString . show) [(1 :: Int) ..]
          combine pf = map $ first (pf `dot`)
          
addQueryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> Query -> Query 
addQueryList f prefix xs = addQuery $ queryList f prefix xs
          
awsBool :: Bool -> B.ByteString
awsBool True = "true"
awsBool False = "false"

awsTrue :: B.ByteString
awsTrue = awsBool True

awsFalse :: B.ByteString
awsFalse = awsBool False

queryToHttpRequest :: Query -> HTTP.Request
queryToHttpRequest Query{..}
    = HTTP.Request {
        HTTP.method = httpMethod method
      , HTTP.secure = case protocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.host = host
      , HTTP.port = port
      , HTTP.path = B.concat $ path : case method of 
                                         Get -> [urlEncodeVarsBS' True subresource query]
                                         PostQuery -> []
      , HTTP.queryString = [] -- not used for safety reasons
      , HTTP.requestHeaders = catMaybes [fmap (\d -> ("Date", fmtRfc822Time d)) date
                                        , fmap (\c -> ("Content-Type", c)) contentType'
                                        , fmap (\md5 -> ("Content-MD5", md5)) contentMd5
                                        , fmap (\auth -> ("Authorization", auth)) authorization]
      , HTTP.requestBody = case method of
                             Get -> L.empty
                             PostQuery -> L.fromChunks [urlEncodeVarsBS' False subresource query]
      }
    where contentType' = case method of
                           PostQuery -> Just "application/x-www-form-urlencoded"
                           _ -> contentType

queryToUri :: Query -> URI
queryToUri Query{..} 
    = nullURI {
        uriScheme = case protocol of
                      HTTP -> "http:"
                      HTTPS -> "https:"
      , uriAuthority = Just URIAuth {
                         uriUserInfo = ""
                       , uriRegName = BU.toString host
                       , uriPort = if port == defaultPort protocol then "" else ':' : show port
                       }
      , uriPath = BU.toString path
      , uriQuery = BU.toString $ urlEncodeVarsBS' True subresource query
      }
