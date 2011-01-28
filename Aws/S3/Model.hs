module Aws.S3.Model
where

import           Data.ByteString.Char8 ({- IsString -})
import           Data.Time

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

data LocationConstraint 
    = EU | UsWest1 | ApSouthEast1 | UsClassic
    deriving (Show)

locationConstraintToId :: LocationConstraint -> String
locationConstraintToId lc
    = case lc of
        EU -> "EU"
        UsWest1 -> "us-west-1"
        ApSouthEast1 -> "ap-southeast-1"
        UsClassic -> ""

idToLocationConstraint :: String -> Maybe LocationConstraint
idToLocationConstraint i    
    = case i of
        "EU" -> Just EU
        "us-west-1" -> Just UsWest1
        "ap-southeast-1" -> Just ApSouthEast1
        "" -> Just UsClassic
        _ -> Nothing
