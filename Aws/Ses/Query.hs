{-# LANGUAGE OverloadedStrings #-}
module Aws.Ses.Query
    ( sesSignQuery
    ) where

import           Aws.Core
import           Aws.Ses.Info
import qualified Data.ByteString                as B
import qualified Network.HTTP.Types             as HTTP


sesSignQuery :: [(B.ByteString, B.ByteString)] -> SesInfo -> SignatureData -> SignedQuery
sesSignQuery query si sd
    = SignedQuery {
        sqMethod        = sesiHttpMethod si
      , sqProtocol      = HTTPS
      , sqHost          = sesiHost si
      , sqPort          = defaultPort HTTPS
      , sqPath          = "/"
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
