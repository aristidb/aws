{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Aws.Metadata
where
  
data SdbMetadata 
    = SdbMetadata {
        requestId :: String
      , boxUsage :: Maybe String
      }
    deriving (Show)

data S3Metadata
    = S3Metadata {
        s3MAmzId2 :: String
      , s3MRequestId :: String
      }
    deriving (Show)

class WithMetadata a m | a -> m where
    getMetadata :: a -> Maybe m
    setMetadata :: m -> a -> a
