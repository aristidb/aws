{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module Aws.S3.GetService
where
  
import           Aws.Response
import           Aws.S3.Error
import           Aws.S3.Info
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.S3.Response
import           Aws.Signature
import           Aws.Transaction
import           Control.Monad.Compose.Class
import           Data.Time.Format
import           System.Locale
import           Text.XML.Monad
import qualified Network.HTTP.Enumerator     as HTTP
import qualified Text.XML.Light              as XL

data GetService = GetService

data GetServiceResponse 
    = GetServiceResponse {
        gsrOwner :: UserInfo
      , gsrBuckets :: [BucketInfo]
      }
    deriving (Show)

instance S3ResponseIteratee GetServiceResponse where
    s3ResponseIteratee = xmlResponseIteratee $ parse <<< parseXmlResponse
        where
          parse :: Xml S3Error XL.Element GetServiceResponse
          parse = do
            owner <- parseUserInfo <<< findElementNameUI "Owner"
            buckets <- inList parseBucket <<< findElementsNameUI "Bucket"
            
            return GetServiceResponse { gsrOwner = owner, gsrBuckets = buckets }
          
          parseUserInfo = do
            id <- strContent <<< findElementNameUI "ID"
            displayName <- strContent <<< findElementNameUI "DisplayName"
            return UserInfo { userId = id, userDisplayName = displayName }

          parseBucket = do
            name <- strContent <<< findElementNameUI "Name"
            creationDateString <- strContent <<< findElementNameUI "CreationDate"
            creationDate <- maybeRaiseXml (EncodingError "Invalid date encoding") $
                            parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" creationDateString
            return BucketInfo { bucketName = name, bucketCreationDate = creationDate }

instance SignQuery GetService where
    type Info GetService = S3Info
    signQuery GetService = s3SignQuery ()

instance Transaction GetService (S3Response GetServiceResponse)
