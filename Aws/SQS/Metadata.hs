module Aws.SQS.Metadata
where

data SqsMetadata
    = SqsMetadata {
        sqsMAmzId2 :: String
      , sqsMRequestId :: String
      }
    deriving (Show)
