{-# LANGUAGE OverloadedStrings #-}
module Aws.Route53.Query
    ( route53SignQuery
    ) where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.Signature
import           Aws.Route53.Info
import           Aws.Util
import qualified Data.ByteString                as B
import qualified Network.HTTP.Types             as HTTP

route53SignQuery :: B.ByteString -> [(B.ByteString, B.ByteString)] -> Route53Info -> SignatureData -> SignedQuery
route53SignQuery path query si sd
    = SignedQuery {
        sqMethod        = Get -- TODO should not be hardcoded
      , sqProtocol      = route53Protocol si
      , sqHost          = route53Endpoint si
      , sqPort          = route53Port si
      , sqPath          = "/2012-02-29" `B.append`  path
      , sqQuery         = HTTP.simpleQueryToQuery query'
      , sqDate          = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType   = Nothing
      , sqContentMd5    = Nothing
      , sqAmzHeaders    = [("X-Amzn-Authorization", authorization)]
      , sqOtherHeaders  = []
      , sqBody          = Nothing
      , sqStringToSign  = stringToSign
      }
    where
      stringToSign  = fmtRfc822Time (signatureTime sd)
      credentials   = signatureCredentials sd
      accessKeyId   = accessKeyID credentials
      authorization = B.concat [ "AWS3-HTTPS AWSAccessKeyId="
                               , accessKeyId
                               , ", Algorithm=HmacSHA256, Signature="
                               , signature credentials HmacSHA256 stringToSign
                               ]
      query' = ("AWSAccessKeyId", accessKeyId) : query
