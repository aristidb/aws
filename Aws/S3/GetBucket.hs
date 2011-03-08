module Aws.S3.GetBucket
where

import Aws.S3.Model

data GetBucket
    = GetBucket {
        gbBucket    :: Bucket
      , gbDelimiter :: Maybe String
      , gbMarker    :: Maybe String
      , gbMaxKeys   :: Maybe Int
      , gbPrefix    :: Maybe String
      }
    deriving (Show)

getBucket :: Bucket -> GetBucket
getBucket bucket 
    = GetBucket { 
        gbBucket    = bucket
      , gbDelimiter = Nothing
      , gbMarker    = Nothing
      , gbMaxKeys   = Nothing
      , gbPrefix    = Nothing
      }

data GetBucketResult
    = GetBucketResult {
        gbrName           :: Bucket
      , gbrDelimiter      :: Maybe String
      , gbrMarker         :: Maybe String
      , gbrMaxKeys        :: Maybe Int
      , gbrPrefix         :: Maybe String
      , gbrContents       :: [ObjectInfo]
      , gbrCommonPrefixes :: [String]
      }
    deriving (Show)
