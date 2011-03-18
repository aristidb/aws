module Aws.S3.Metadata
where

data S3Metadata
    = S3Metadata {
        s3MAmzId2 :: String
      , s3MRequestId :: String
      }
    deriving (Show)
