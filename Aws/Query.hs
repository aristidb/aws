{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeFamilies, OverloadedStrings #-}

module Aws.Query
where

import           Aws.Http
import           Aws.Util
import           Control.Arrow
import           Data.Maybe
import           Data.Time
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as L
import qualified Data.ByteString.UTF8    as BU
import qualified Network.HTTP.Enumerator as HTTP

data SignedQuery 
    = SignedQuery {
        sqMethod :: Method
      , sqProtocol :: Protocol
      , sqHost :: B.ByteString
      , sqPort :: Int
      , sqPath :: B.ByteString
      , sqSubresource :: Maybe B.ByteString
      , sqQuery :: [(B.ByteString, B.ByteString)]
      , sqDate :: Maybe UTCTime
      , sqAuthorization :: Maybe B.ByteString
      , sqContentType :: Maybe B.ByteString
      , sqContentMd5 :: Maybe B.ByteString
      , sqBody :: L.ByteString
      , sqStringToSign :: B.ByteString
      }
    deriving (Show)

addQuery :: [(B.ByteString, B.ByteString)] -> SignedQuery -> SignedQuery
addQuery xs q = q { sqQuery = xs ++ sqQuery q }

addQueryItem :: B.ByteString -> B.ByteString -> SignedQuery -> SignedQuery
addQueryItem name value = addQuery [(name, value)]

addQueryIf :: Bool -> [(B.ByteString, B.ByteString)] -> SignedQuery -> SignedQuery
addQueryIf True  = addQuery
addQueryIf False = const id

addQueryUnless :: Bool -> [(B.ByteString, B.ByteString)] -> SignedQuery -> SignedQuery
addQueryUnless = addQueryIf . not
      
addQueryMaybe :: (a -> B.ByteString) -> (B.ByteString, Maybe a) -> SignedQuery -> SignedQuery
addQueryMaybe f (name, Just a) q = q { sqQuery = (name, f a) : sqQuery q }
addQueryMaybe _ (_, Nothing) q = q

dot :: B.ByteString -> B.ByteString -> B.ByteString
dot x y = B.concat [x, BU.fromString ".", y]

queryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> [(B.ByteString, B.ByteString)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . BU.fromString . show) [(1 :: Int) ..]
          combine pf = map $ first (pf `dot`)

addQueryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> SignedQuery -> SignedQuery 
addQueryList f prefix xs = addQuery $ queryList f prefix xs
          
awsBool :: Bool -> B.ByteString
awsBool True = "true"
awsBool False = "false"

awsTrue :: B.ByteString
awsTrue = awsBool True

awsFalse :: B.ByteString
awsFalse = awsBool False

queryToHttpRequest :: SignedQuery -> HTTP.Request
queryToHttpRequest SignedQuery{..}
    = HTTP.Request {
        HTTP.method = httpMethod sqMethod
      , HTTP.secure = case sqProtocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.host = sqHost
      , HTTP.port = sqPort
      , HTTP.path = B.concat $ sqPath : case sqMethod of 
                                         Get -> [urlEncodeVarsBS' True sqSubresource sqQuery]
                                         PostQuery -> []
      , HTTP.queryString = [] -- not used for safety reasons
      , HTTP.requestHeaders = catMaybes [fmap (\d -> ("Date", fmtRfc822Time d)) sqDate
                                        , fmap (\c -> ("Content-Type", c)) contentType
                                        , fmap (\md5 -> ("Content-MD5", md5)) sqContentMd5
                                        , fmap (\auth -> ("Authorization", auth)) sqAuthorization]
      , HTTP.requestBody = case sqMethod of
                             Get -> L.empty
                             PostQuery -> L.fromChunks [urlEncodeVarsBS' False sqSubresource sqQuery]
      }
    where contentType = case sqMethod of
                           PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"
                           _ -> sqContentType

queryToUri :: SignedQuery -> B.ByteString
queryToUri SignedQuery{..} 
    = B.concat [
       case sqProtocol of
         HTTP -> "http://"
         HTTPS -> "https://"
      , sqHost
      , if sqPort == defaultPort sqProtocol then "" else BU.fromString $ ':' : show sqPort
      , sqPath
      , urlEncodeVarsBS' True sqSubresource sqQuery
      ]
