{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module Aws.S3.Commands.GetService
where
  
import           Aws.Response
import           Aws.S3.Error
import           Aws.S3.Info
import           Aws.S3.Metadata
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.S3.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import           Text.XML.Enumerator.Cursor (($/), ($//), (&|))
import qualified Text.XML.Enumerator.Cursor as Cu

data GetService = GetService

data GetServiceResponse 
    = GetServiceResponse {
        gsrOwner :: UserInfo
      , gsrBuckets :: [BucketInfo]
      }
    deriving (Show)

instance ResponseIteratee GetServiceResponse where
    type ResponseMetadata GetServiceResponse = S3Metadata

    responseIteratee = s3ResponseIteratee $ xmlCursorIteratee parse
        where
          parse el = do
            owner <- s3ForceM "Missing Owner" $ el $/ Cu.laxElement "Owner" &| parseUserInfo
            buckets <- sequence $ el $// Cu.laxElement "Bucket" &| parseBucket
            return GetServiceResponse { gsrOwner = owner, gsrBuckets = buckets }
          
          parseBucket el = do
            name <- s3Force "Missing owner Name" $ el $/ elCont "Name"
            creationDateString <- s3Force "Missing owner CreationDate" $ el $/ elCont "CreationDate"
            creationDate <- s3Force "Invalid CreationDate" . maybeToList $ parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" creationDateString
            return BucketInfo { bucketName = name, bucketCreationDate = creationDate }

instance SignQuery GetService where
    type Info GetService = S3Info
    signQuery GetService = s3SignQuery S3Query { s3QBucket = Nothing, s3QSubresources = [], s3QQuery = [] }

instance Transaction GetService GetServiceResponse
