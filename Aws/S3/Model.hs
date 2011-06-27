{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Aws.S3.Model
where

import           Aws.S3.Error
import           Aws.S3.Response
import           Aws.Xml
import           Data.Time
import           System.Locale
import           Text.XML.Enumerator.Cursor (($/), (&|))
import qualified Control.Failure            as F
import qualified Text.XML.Enumerator.Cursor as Cu

type CanonicalUserId = String

data UserInfo
    = UserInfo {
        userId :: CanonicalUserId
      , userDisplayName :: String
      }
    deriving (Show)

parseUserInfo :: F.Failure XmlException m => Cu.Cursor -> m UserInfo
parseUserInfo el = do id_ <- force "Missing user ID" $ el $/ elCont "ID"
                      displayName <- force "Missing user DisplayName" $ el $/ elCont "DisplayName"
                      return UserInfo { userId = id_, userDisplayName = displayName }

type Bucket = String

data BucketInfo
    = BucketInfo {
        bucketName :: Bucket
      , bucketCreationDate :: UTCTime
      }
    deriving (Show)

data ObjectInfo
    = ObjectInfo {
        objectKey :: String
      , objectLastModified :: UTCTime
      , objectETag :: String
      , objectSize :: Integer
      , objectStorageClass :: String
      , objectOwner :: UserInfo
      }
    deriving (Show)

parseObjectInfo :: F.Failure XmlException m => Cu.Cursor -> m ObjectInfo
parseObjectInfo el 
    = do key <- force "Missing object Key" $ el $/ elCont "Key"
         let time s = case parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s of
                        Nothing -> F.failure $ XmlException "Invalid time"
                        Just v -> return v
         lastModified <- forceM "Missing object LastModified" $ el $/ elCont "LastModified" &| time
         eTag <- force "Missing object ETag" $ el $/ elCont "ETag"
         size <- forceM "Missing object Size" $ el $/ elCont "Size" &| readInt
         storageClass <- force "Missing object StorageClass" $ el $/ elCont "StorageClass"
         owner <- forceM "Missing object Owner" $ el $/ Cu.laxElement "Owner" &| parseUserInfo
         return ObjectInfo{
                      objectKey          = key
                    , objectLastModified = lastModified
                    , objectETag         = eTag
                    , objectSize         = size
                    , objectStorageClass = storageClass
                    , objectOwner        = owner
                    }

type LocationConstraint = String

locationUsClassic, locationUsWest, locationEu, locationApSouthEast, locationApNorthEast :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationEu = "EU"
locationApSouthEast = "ap-southeast-1"
locationApNorthEast = "ap-northeast-1"
