module Aws.S3.Commands.GetBucketLocation
       where

import           Aws.Core
import           Aws.S3.Core

import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Types as HTTP
import           Text.XML.Cursor (($.//))

data GetBucketLocation
  = GetBucketLocation {
      gblBucket :: Bucket
    } deriving Show

getBucketLocation :: Bucket -> GetBucketLocation
getBucketLocation bucket
  = GetBucketLocation {
      gblBucket = bucket
    }

data GetBucketLocationResponse
  = GetBucketLocationResponse { gblrLocationConstraint :: LocationConstraint }
    deriving Show

instance SignQuery GetBucketLocation where
  type ServiceConfiguration GetBucketLocation = S3Configuration
  signQuery GetBucketLocation {..} = s3SignQuery S3Query {
                                       s3QMethod = Get
                                     , s3QBucket = Just $ T.encodeUtf8 gblBucket
                                     , s3QObject = Nothing
                                     , s3QSubresources = [("location" :: B8.ByteString, Nothing :: Maybe B8.ByteString)]
                                     , s3QQuery = HTTP.toQuery ([] :: [(B8.ByteString, T.Text)]) 
                                     , s3QContentType = Nothing
                                     , s3QContentMd5 = Nothing
                                     , s3QAmzHeaders = []
                                     , s3QOtherHeaders = []
                                     , s3QRequestBody = Nothing
                                     }

instance ResponseConsumer r GetBucketLocationResponse where
  type ResponseMetadata GetBucketLocationResponse = S3Metadata

  responseConsumer _ = s3XmlResponseConsumer parse
    where parse cursor = do
            locationConstraint <- force "Missing Location" $ cursor $.// elContent "LocationConstraint"
            return GetBucketLocationResponse { gblrLocationConstraint = locationConstraint }

instance Transaction GetBucketLocation GetBucketLocationResponse

instance AsMemoryResponse GetBucketLocationResponse where
  type MemoryResponse GetBucketLocationResponse = GetBucketLocationResponse
  loadToMemory = return
