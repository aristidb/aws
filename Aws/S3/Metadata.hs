{-# LANGUAGE DeriveDataTypeable #-}
module Aws.S3.Metadata
where
  
import Control.Monad
import Data.Monoid
import Data.Typeable

data S3Metadata
    = S3Metadata {
        s3MAmzId2 :: Maybe String
      , s3MRequestId :: Maybe String
      }
    deriving (Show, Typeable)

instance Monoid S3Metadata where
    mempty = S3Metadata Nothing Nothing
    S3Metadata a1 r1 `mappend` S3Metadata a2 r2 = S3Metadata (a1 `mplus` a2) (r1 `mplus` r2)
