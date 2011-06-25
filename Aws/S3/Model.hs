{-# LANGUAGE OverloadedStrings #-}
module Aws.S3.Model
where

import           Aws.S3.Error
import           Aws.S3.Response
import           Aws.Xml
import           Data.Time
import           System.Locale
import           Text.XML.Enumerator.Cursor (($/), (&|))
import qualified Text.XML.Enumerator.Cursor as Cu

type CanonicalUserId = String

data UserInfo
    = UserInfo {
        userId :: CanonicalUserId
      , userDisplayName :: String
      }
    deriving (Show)

parseUserInfo :: Cu.Cursor -> Either (S3Error ()) UserInfo
parseUserInfo el = do id_ <- s3Force "Missing user ID" $ el $/ elCont "ID"
                      displayName <- s3Force "Missing user DisplayName" $ el $/ elCont "DisplayName"
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

parseObjectInfo :: Cu.Cursor -> Either (S3Error ()) ObjectInfo
parseObjectInfo el 
    = do key <- s3Force "Missing object Key" $ el $/ elCont "Key"
         let time s = case parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" s of
                        Nothing -> Left $ S3XmlError "Invalid time" ()
                        (Just v) -> Right v
         lastModified <- s3ForceM "Missing object LastModified" $ el $/ elCont "LastModified" &| time
         eTag <- s3Force "Missing object ETag" $ el $/ elCont "ETag"
         size <- s3ForceM "Missing object Size" $ el $/ elCont "Size" &| s3ReadInt
         storageClass <- s3Force "Missing object StorageClass" $ el $/ elCont "StorageClass"
         owner <- s3ForceM "Missing object Owner" $ el $/ Cu.laxElement "Owner" &| parseUserInfo
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
