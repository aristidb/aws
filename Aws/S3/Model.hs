{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Aws.S3.Model
where

import           Aws.Xml
import           Data.Time
import           System.Locale
import           Text.XML.Enumerator.Cursor (($/), (&|), (>=>))
import qualified Control.Failure            as F
import qualified Text.XML.Enumerator.Cursor as Cu
import qualified Data.Text                  as T

type CanonicalUserId = T.Text

data UserInfo
    = UserInfo {
        userId          :: CanonicalUserId
      , userDisplayName :: T.Text
      }
    deriving (Show)

parseUserInfo :: F.Failure XmlException m => Cu.Cursor -> m UserInfo
parseUserInfo el = do id_ <- force "Missing user ID" $ el $/ elContent "ID"
                      displayName <- force "Missing user DisplayName" $ el $/ elContent "DisplayName"
                      return UserInfo { userId = id_, userDisplayName = displayName }

data CannedAcl
    = AclPrivate 
    | AclPublicRead 
    | AclPublicReadWrite 
    | AclAuthenticatedRead 
    | AclBucketOwnerRead 
    | AclBucketOwnerFullControl
    | AclLogDeliveryWrite
    deriving (Show)

writeCannedAcl :: CannedAcl -> T.Text
writeCannedAcl AclPrivate                = "private"
writeCannedAcl AclPublicRead             = "public-read"
writeCannedAcl AclPublicReadWrite        = "public-read-write"
writeCannedAcl AclAuthenticatedRead      = "authenticated-read"
writeCannedAcl AclBucketOwnerRead        = "bucket-owner-read"
writeCannedAcl AclBucketOwnerFullControl = "bucket-owner-full-control"
writeCannedAcl AclLogDeliveryWrite       = "log-delivery-write"

type Bucket = T.Text

data BucketInfo
    = BucketInfo {
        bucketName         :: Bucket
      , bucketCreationDate :: UTCTime
      }
    deriving (Show)

data ObjectInfo
    = ObjectInfo {
        objectKey          :: T.Text
      , objectLastModified :: UTCTime
      , objectETag         :: T.Text
      , objectSize         :: Integer
      , objectStorageClass :: T.Text
      , objectOwner        :: UserInfo
      }
    deriving (Show)

parseObjectInfo :: F.Failure XmlException m => Cu.Cursor -> m ObjectInfo
parseObjectInfo el 
    = do key <- force "Missing object Key" $ el $/ elContent "Key"
         let time s = case parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ T.unpack s of
                        Nothing -> F.failure $ XmlException "Invalid time"
                        Just v -> return v
         lastModified <- forceM "Missing object LastModified" $ el $/ Cu.laxElement "LastModified" >=> Cu.content &| time
         eTag <- force "Missing object ETag" $ el $/ elContent "ETag"
         size <- forceM "Missing object Size" $ el $/ elContent "Size" &| textReadInt
         storageClass <- force "Missing object StorageClass" $ el $/ elContent "StorageClass"
         owner <- forceM "Missing object Owner" $ el $/ Cu.laxElement "Owner" &| parseUserInfo
         return ObjectInfo{
                      objectKey          = key
                    , objectLastModified = lastModified
                    , objectETag         = eTag
                    , objectSize         = size
                    , objectStorageClass = storageClass
                    , objectOwner        = owner
                    }

type LocationConstraint = T.Text

locationUsClassic, locationUsWest, locationEu, locationApSouthEast, locationApNorthEast :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationEu = "EU"
locationApSouthEast = "ap-southeast-1"
locationApNorthEast = "ap-northeast-1"
