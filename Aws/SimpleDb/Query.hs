{-# LANGUAGE OverloadedStrings #-}
module Aws.SimpleDb.Query
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Util
import           Data.List
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types   as HTTP

sdbSignQuery :: [(B.ByteString, B.ByteString)] -> SdbInfo -> SignatureData -> SignedQuery
sdbSignQuery q si sd
    = SignedQuery {
        sqMethod = method
      , sqProtocol = sdbiProtocol si
      , sqHost = host
      , sqPort = sdbiPort si
      , sqPath = path
      , sqQuery = sq
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      , sqBody = L.empty
      , sqStringToSign = stringToSign
      }
    where
      ah = HmacSHA256
      q' = HTTP.simpleQueryToQuery . sort $ q ++ ("Version", "2009-04-15") : queryAuth
      ti = signatureTimeInfo sd
      cr = signatureCredentials sd
      queryAuth = [case ti of
                     AbsoluteTimestamp time -> 
                         ("Timestamp", fmtAmzTime time)
                     AbsoluteExpires time -> 
                         ("Expires", fmtAmzTime time)
                  , ("AWSAccessKeyId", accessKeyID cr)
                  , ("SignatureMethod", amzHash ah)
                  , ("SignatureVersion", "2")
                  ]
      sq = ("Signature", Just sig) : q'
      method = sdbiHttpMethod si
      host = sdbiHost si
      path = "/"
      sig = signature cr ah stringToSign
      stringToSign = B.intercalate "\n" [httpMethod method
                                        , host
                                        , path
                                        , HTTP.renderQuery False q']
