{-# LANGUAGE OverloadedStrings #-}
module Aws.SimpleDb.Query
where

import           Aws.Http
import           Aws.Query
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.Util
import           Data.List
import qualified Data.ByteString   as B

{-
sdbiBaseQuery :: SdbInfo -> SignedQuery
sdbiBaseQuery SdbInfo{..} = SignedQuery { 
                              method = sdbiHttpMethod
                            , protocol = sdbiProtocol
                            , host = sdbiHost
                            , port = sdbiPort 
                            , path = "/"
                            , query = [("Version", "2009-04-15")]
                            }
-}

sdbSignQuery :: [(B.ByteString, B.ByteString)] -> SdbInfo -> SignatureData -> SignedQuery
sdbSignQuery q si sd
    = SignedQuery {
        sqMethod = method
      , sqProtocol = sdbiProtocol si
      , sqHost = host
      , sqPort = sdbiPort si
      , sqPath = path
      , sqSubresource = Nothing
      , sqQuery = q'
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      , sqBody = ""
      , sqStringToSign = stringToSign
      }
    where
      ah = HmacSHA256
      q' = sort $ q ++ ("Version", "2009-04-15") : authorizationQueryPrepare SimpleDB ah sd
      method = sdbiHttpMethod si
      host = sdbiHost si
      path = "/"
      sig = signature (signatureCredentials sd) ah stringToSign
      stringToSign = B.intercalate "\n" [httpMethod method
                                        , host
                                        , path
                                        , urlEncodeVarsBS False q']
