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
    = undefined
    where
      q' = sort $ q ++ ("Version", "2009-04-15") : authorizationQueryPrepare SimpleDB HmacSHA256 sd
      method = sdbiHttpMethod si
      host = sdbiHost si
      path = "/"
      sig = signature (signatureCredentials sd) HmacSHA256 stringToSign
      stringToSign = B.intercalate "\n" [httpMethod method
                                        , host
                                        , path
                                        , urlEncodeVarsBS False q']
