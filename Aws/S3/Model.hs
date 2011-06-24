{-# LANGUAGE OverloadedStrings #-}
module Aws.S3.Model
where

import           Aws.S3.Error
import           Aws.Xml
import           Data.Time
import           Text.XML.Enumerator.Cursor (($/))
import qualified Text.XML.Enumerator.Cursor as Cu

type CanonicalUserId = String

data UserInfo
    = UserInfo {
        userId :: CanonicalUserId
      , userDisplayName :: String
      }
    deriving (Show)

parseUserInfo :: Cu.Cursor -> Either S3Error UserInfo
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
      , objectStorageClass :: StorageClass
      , objectOwner :: UserInfo
      }
    deriving (Show)

data StorageClass 
    = StorageClassStandard
    deriving (Show)

type LocationConstraint = String

locationUsClassic, locationUsWest, locationEu, locationApSouthEast, locationApNorthEast :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationEu = "EU"
locationApSouthEast = "ap-southeast-1"
locationApNorthEast = "ap-northeast-1"
