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

{-
data LocationConstraint 
    = EU | UsWest1 | ApSouthEast1 | UsClassic
    deriving (Show)
-}

$(makeId "LocationConstraint" [|id|]
             [
               ("EU", "EU")
             , ("UsWest1", "us-west-1")
             , ("ApSouthEast1", "ap-southeast-1")
             , ("UsClassic", "")
             ])
