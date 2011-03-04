{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Aws.S3.Response
where

import           Aws.Response
import           Aws.S3.Error
import           Data.Maybe
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator      as En

data S3Response a
    = S3Response {
        fromS3Response :: a
      , s3AmzId2 :: String
      , s3RequestId :: String
      }
    deriving (Show)

instance ResponseIteratee (S3Response ()) where
    responseIteratee status headers = do
      let headerString = fromMaybe "" . fmap BU.toString . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      return $ S3Response {
                   fromS3Response = ()
                 , s3AmzId2 = amzId2
                 , s3RequestId = requestId
                 }
