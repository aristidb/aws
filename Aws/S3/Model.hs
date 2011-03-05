{-# LANGUAGE TemplateHaskell #-}
module Aws.S3.Model
where

import Aws.Id
import Data.ByteString.Char8 ({- IsString -})
import Data.Time

type CanonicalUserId = String

data UserInfo
    = UserInfo {
        userId :: CanonicalUserId
      , userDisplayName :: String
      }
    deriving (Show)

type Bucket = String

data BucketInfo
    = BucketInfo {
        bucketName :: Bucket
      , bucketCreationDate :: UTCTime
      }
    deriving (Show)

type LocationConstraint = String

locationUsClassic = ""
locationUsWest = "us-west-1"
locationEU = "EU"
locationApSouthEast = "ap-southeast-1"
locationApNorthEast = "ap-northeast-1"
