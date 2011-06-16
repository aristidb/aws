{-# LANGUAGE OverloadedStrings #-}

module Aws.SQS.Query where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Signature
import           Aws.SQS.Info
import           Aws.Util
import           Data.List
import           Data.Monoid
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import qualified Network.HTTP.Types             as HTTP

sqsSignQuery :: [(B.ByteString, B.ByteString)] -> SqsInfo -> SignatureData -> SignedQuery
sqsSignQuery q si sd
    = SignedQuery {
        sqMethod = method
      , sqProtocol = sdbiProtocol si
      , sqHost = host
      , sqPort = sqsiPort si
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
      q' = HTTP.simpleQueryToQuery . sort $ q ++ ("Version", "2009-02-01") : queryAuth
      ti = signatureTimeInfo sd
      cr = signatureCredentials sd
      queryAuth = [case ti of
                     AbsoluteTimestamp time -> ("Timestamp", fmtAmzTime time)
                     AbsoluteExpires   time -> ("Expires", fmtAmzTime time)
                  , ("AWSAccessKeyId", accessKeyID cr)
                  , ("SignatureMethod", amzHash ah)
                  , ("SignatureVersion", "2")
                  ]
      sq = ("Signature", Just sig) : q'
      method = sqsiHttpMethod si
      host = sqsiHost si
      path = "/"
      sig = signature cr ah stringToSign
      stringToSign = Blaze.toByteString . mconcat $ 
                     intersperse (Blaze8.fromChar '\n')
                       [Blaze.copyByteString $ httpMethod method
                       , Blaze.copyByteString $ host
                       , Blaze.copyByteString $ path
                       , HTTP.renderQueryBuilder False q']
