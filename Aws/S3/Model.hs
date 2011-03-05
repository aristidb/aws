{-# LANGUAGE TemplateHaskell #-}
module Aws.S3.Model
where

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

locationUsClassic, locationUsWest, locationEu, locationApSouthEast, locationApNorthEast :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationEu = "EU"
locationApSouthEast = "ap-southeast-1"
locationApNorthEast = "ap-northeast-1"
