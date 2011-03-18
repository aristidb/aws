{-# LANGUAGE FlexibleInstances, OverloadedStrings, ScopedTypeVariables #-}
module Aws.S3.Response
where

import           Aws.Metadata
import           Aws.Response
import           Aws.S3.Error
import           Aws.Util
import           Control.Applicative
import           Control.Exception
import           Data.Maybe
import qualified Data.Ascii              as A
import qualified Data.ByteString         as B
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP
import qualified Network.HTTP.Types      as HTTP

data S3Response a
    = S3Response {
        fromS3Response :: a
      , s3AmzId2 :: String
      , s3RequestId :: String
      }
    deriving (Show)

instance (S3ResponseIteratee a) => ResponseIteratee (S3Response a) where
    responseIteratee status headers = do
      let headerString = fromMaybe "" . fmap A.toString . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      
      specific <- tryError $ s3ResponseIteratee status headers
      
      case specific of
        Left (err :: S3Error) -> En.throwError (setMetadata m err)
            where m = S3Metadata { s3MAmzId2 = amzId2, s3MRequestId = requestId }
        Right resp -> return S3Response {
                                        fromS3Response = resp
                                      , s3AmzId2 = amzId2
                                      , s3RequestId = requestId
                                      }

class S3ResponseIteratee a where
    s3ResponseIteratee :: HTTP.Status -> HTTP.ResponseHeaders -> En.Iteratee B.ByteString IO a

instance S3ResponseIteratee HTTP.Response where
    s3ResponseIteratee = HTTP.lbsIter
