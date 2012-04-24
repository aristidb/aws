{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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

route53SignQuery :: Method -> B.ByteString -> [(B.ByteString, B.ByteString)] -> Route53Info -> SignatureData -> SignedQuery
route53SignQuery method resource query Route53Info{..} sd
    = SignedQuery {
        sqMethod        = method
      , sqProtocol      = route53Protocol 
      , sqHost          = route53Endpoint
      , sqPort          = route53Port
      , sqPath          = route53ApiVersion `B.append` resource
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
