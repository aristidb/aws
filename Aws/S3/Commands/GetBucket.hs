{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings #-}
module Aws.S3.Commands.GetBucket
where

import           Aws.S3.Info
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.Signature
import           Control.Applicative
import           Control.Arrow         (second)
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import qualified Data.Ascii            as A
import qualified Data.ByteString.UTF8  as BU
import qualified Network.HTTP.Types    as HTTP

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

instance SignQuery GetBucket where
    type Info GetBucket = S3Info
    signQuery GetBucket {..} = s3SignQuery S3Query { 
                                 s3QBucket = Just $ A.unsafeFromString gbBucket
                               , s3QSubresources = []
                               , s3QQuery = HTTP.simpleQueryToQuery $ map (second BU.fromString) $ catMaybes [
                                              ("delimiter",) <$> gbDelimiter
                                            , ("marker",) <$> gbMarker
                                            , ("max-keys",) <$> show <$> gbMaxKeys
                                            , ("prefix",) <$> gbPrefix
                                            ]
                               }
