{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module Aws.S3.Response
where

import           Aws.Response
import           Data.Maybe
import qualified Data.ByteString         as B
import qualified Data.ByteString.UTF8    as BU
import qualified Data.Enumerator         as En
import qualified Network.HTTP.Enumerator as HTTP
import qualified Network.Wai             as Wai

data S3Response a
    = S3Response {
        fromS3Response :: a
      , s3AmzId2 :: String
      , s3RequestId :: String
      }
    deriving (Show)

instance (S3ResponseIteratee a) => ResponseIteratee (S3Response a) where
    responseIteratee status headers = do
      specific <- s3ResponseIteratee status headers
      let headerString = fromMaybe "" . fmap BU.toString . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"
      return $ S3Response {
                   fromS3Response = specific
                 , s3AmzId2 = amzId2
                 , s3RequestId = requestId
                 }

class S3ResponseIteratee a where
    s3ResponseIteratee :: Wai.Status -> HTTP.Headers -> En.Iteratee B.ByteString IO a

instance S3ResponseIteratee HTTP.Response where
    s3ResponseIteratee = HTTP.lbsIter
