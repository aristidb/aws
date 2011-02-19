{-# LANGUAGE OverloadedStrings #-}

module Aws.S3.Query
where

import           Aws.Credentials
import           Aws.Http
import           Aws.Query
import           Aws.S3.Info
import           Aws.Signature
import           Aws.Util
import           Data.Maybe
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

s3SignQuery :: () -> S3Info -> SignatureData -> SignedQuery
s3SignQuery x si sd 
    = SignedQuery {
        sqMethod = Get
      , sqProtocol = s3Protocol si
      , sqHost = s3Host si
      , sqPort = s3Port si
      , sqPath = path
      , sqSubresource = Nothing
      , sqQuery = []
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Just $ B.concat [
                           "AWS "
                          , accessKeyID cr
                          , ":"
                          , sig
                          ]
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      , sqBody = L.empty
      , sqStringToSign = stringToSign
      }
    where
      method = Get
      contentMd5 = Nothing
      contentType = Nothing
      path = "/"
      canonicalizedResource = "/"
      ti = signatureTimeInfo sd
      cr = signatureCredentials sd
      sig = signature cr HmacSHA1 stringToSign
      stringToSign = B.intercalate "\n" $ concat [[httpMethod method]
                                                 , [fromMaybe "" contentMd5]
                                                 , [fromMaybe "" contentType]
                                                 , [case ti of
                                                      AbsoluteTimestamp time -> fmtRfc822Time time
                                                      AbsoluteExpires time -> fmtTimeEpochSeconds time]
                                                 , [] -- canonicalized AMZ headers
                                                 , [canonicalizedResource]]

{-
instance AsQuery GetService where
    type Info GetService = () -- < preliminary
    asQuery _ _ = Query {
                    api = S3
                  , method = Get
                  , protocol = HTTP
                  , host = "s3.amazonaws.com"
                  , port = 80
                  , path = "/"
                  , canonicalizedResource = "/"
                  , subresource = Nothing
                  , query = []
                  , date = Nothing
                  , authorization = Nothing
                  , contentType = Nothing
                  , contentMd5 = Nothing
                  , body = ""
                  , stringToSign = Nothing
                  }
-}