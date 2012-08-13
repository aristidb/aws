module Aws.S3.Commands.GetService
where

import           Aws.Core
import           Aws.S3.Core
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import           Text.XML.Cursor  (($/), ($//), (&|))
import qualified Data.Text        as T
import qualified Text.XML.Cursor  as Cu

data GetService = GetService

data GetServiceResponse
    = GetServiceResponse {
        gsrOwner :: UserInfo
      , gsrBuckets :: [BucketInfo]
      }
    deriving (Show)

instance ResponseConsumer r GetServiceResponse where
    type ResponseMetadata GetServiceResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where
          parse el = do
            owner <- forceM "Missing Owner" $ el $/ Cu.laxElement "Owner" &| parseUserInfo
            buckets <- sequence $ el $// Cu.laxElement "Bucket" &| parseBucket
            return GetServiceResponse { gsrOwner = owner, gsrBuckets = buckets }

          parseBucket el = do
            name <- force "Missing owner Name" $ el $/ elContent "Name"
            creationDateString <- force "Missing owner CreationDate" $ el $/ elContent "CreationDate" &| T.unpack
            creationDate <- force "Invalid CreationDate" . maybeToList $ parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" creationDateString
            return BucketInfo { bucketName = name, bucketCreationDate = creationDate }

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetService where
    type ServiceConfiguration GetService = S3Configuration
    signQuery GetService = s3SignQuery S3Query {
                                s3QMethod = Get
                              , s3QBucket = Nothing
                              , s3QObject = Nothing
                              , s3QSubresources = []
                              , s3QQuery = []
                              , s3QContentType = Nothing
                              , s3QContentMd5 = Nothing
                              , s3QAmzHeaders = []
                              , s3QOtherHeaders = []
                              , s3QRequestBody = Nothing
                              }

instance Transaction GetService GetServiceResponse

instance AsMemoryResponse GetServiceResponse where
  type MemoryResponse GetServiceResponse = GetServiceResponse
  loadToMemory = return