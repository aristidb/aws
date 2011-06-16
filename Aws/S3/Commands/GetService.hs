{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
module Aws.S3.Commands.GetService
where
  
import           Aws.Response
import           Aws.S3.Error
import           Aws.S3.Info
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.S3.Response
import           Aws.Signature
import           Aws.Transaction
import           Control.Monad
import           Data.Enumerator              ((=$))
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|))
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML

data GetService = GetService

data GetServiceResponse 
    = GetServiceResponse {
        gsrOwner :: UserInfo
      , gsrBuckets :: [BucketInfo]
      }
    deriving (Show)

instance S3ResponseIteratee GetServiceResponse where
    s3ResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                           let cursor = Cu.fromDocument doc
                                           case parse cursor of                                  
                                             Left err -> En.throwError err
                                             Right v -> return v
        where
          parse :: Cu.Cursor -> Either S3Error GetServiceResponse
          parse el = do
            owner <- xmlForce "Missing Owner" <=< sequence $ el $/ Cu.laxElement "Owner" &| parseUserInfo
            buckets <- sequence $ el $// Cu.laxElement "Bucket" &| parseBucket
            return GetServiceResponse { gsrOwner = owner, gsrBuckets = buckets }
          
          parseBucket :: Cu.Cursor -> Either S3Error BucketInfo
          parseBucket el = do
            name <- xmlForce "Missing owner Name" $ el $/ Cu.laxElement "Name" &/ Cu.content &| T.unpack
            creationDateString <- xmlForce "Missing owner CreationDate" $ el $/ Cu.laxElement "CreationDate" &/ Cu.content &| T.unpack
            creationDate <- xmlForce "Invalid CreationDate" . maybeToList $ parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" creationDateString
            return BucketInfo { bucketName = name, bucketCreationDate = creationDate }

instance SignQuery GetService where
    type Info GetService = S3Info
    signQuery GetService = s3SignQuery S3Query { s3QBucket = Nothing, s3QSubresources = [], s3QQuery = [] }

instance Transaction GetService (S3Response GetServiceResponse)
