{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Aws.Query
where

import           Aws.Http
import           Aws.Util
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import 	         Network.TLS
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString          as B
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Network.HTTP.Enumerator  as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Network.TLS              as TLS

data SignedQuery 
    = SignedQuery {
        sqMethod :: Method
      , sqProtocol :: Protocol
      , sqHost :: B.ByteString
      , sqPort :: Int
      , sqPath :: B.ByteString
      , sqQuery :: HTTP.Query
      , sqDate :: Maybe UTCTime
      , sqAuthorization :: Maybe B.ByteString
      , sqContentType :: Maybe B.ByteString
      , sqContentMd5 :: Maybe B.ByteString
      , sqAmzHeaders :: HTTP.RequestHeaders
      , sqBody :: Maybe (HTTP.RequestBody IO)
      , sqStringToSign :: B.ByteString
      }
    --deriving (Show)

queryToHttpRequest :: SignedQuery -> HTTP.Request IO
queryToHttpRequest SignedQuery{..}
    = HTTP.Request {
        HTTP.method = httpMethod sqMethod
      , HTTP.secure = case sqProtocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.checkCerts = const (return TLS.CertificateUsageAccept) -- FIXME: actually check certificates
      , HTTP.host = sqHost
      , HTTP.port = sqPort
      , HTTP.path = sqPath
      , HTTP.queryString = sqQuery
      , HTTP.requestHeaders = catMaybes [fmap (\d -> ("Date", fmtRfc822Time d)) sqDate
                                        , fmap (\c -> ("Content-Type", c)) contentType
                                        , fmap (\md5 -> ("Content-MD5", md5)) sqContentMd5
                                        , fmap (\auth -> ("Authorization", auth)) sqAuthorization]
                              ++ sqAmzHeaders
      , HTTP.requestBody = case sqMethod of
                             PostQuery -> HTTP.RequestBodyLBS . Blaze.toLazyByteString $ HTTP.renderQueryBuilder False sqQuery
                             _         -> case sqBody of
                                            Nothing -> HTTP.RequestBodyBuilder 0 mempty
                                            Just x  -> x
      , HTTP.proxy = Nothing
      , HTTP.rawBody = False
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
      , if sqPort == defaultPort sqProtocol then "" else T.encodeUtf8 . T.pack $ ':' : show sqPort
      , sqPath
      , HTTP.renderQuery True sqQuery
      ]
