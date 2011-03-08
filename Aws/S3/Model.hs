module Aws.S3.Model
where

import           Aws.S3.Error
import           Control.Monad.Compose.Class
import           Data.Time
import           Text.XML.Monad
import qualified Text.XML.Light              as XL

type CanonicalUserId = String

data UserInfo
    = UserInfo {
        userId :: CanonicalUserId
      , userDisplayName :: String
      }
    deriving (Show)

parseUserInfo :: Xml S3Error XL.Element UserInfo
parseUserInfo = do
  id_ <- strContent <<< findElementNameUI "ID"
  displayName <- strContent <<< findElementNameUI "DisplayName"
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
