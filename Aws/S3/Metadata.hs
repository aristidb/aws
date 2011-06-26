{-# LANGUAGE DeriveDataTypeable #-}
module Aws.S3.Metadata
where
  
import Aws.Response
import Data.Typeable

data S3Metadata
    = S3Metadata {
        s3MAmzId2 :: Maybe String
      , s3MRequestId :: Maybe String
      }
    deriving (Show, Typeable)

instance Metadata S3Metadata where
    emptyMetadata = S3Metadata Nothing Nothing