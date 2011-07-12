{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances #-}
module Aws.S3.Commands.GetBucket
where

import           Aws.Http
import           Aws.Response
import           Aws.S3.Info
import           Aws.S3.Metadata
import           Aws.S3.Model
import           Aws.S3.Query
import           Aws.S3.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Control.Arrow              (second)
import           Data.ByteString.Char8      ({- IsString -})
import           Data.Maybe
import           Text.XML.Enumerator.Cursor (($/), (&|), (&//))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Traversable
import qualified Network.HTTP.Types         as HTTP
import qualified Text.XML.Enumerator.Cursor as Cu

data GetBucket
    = GetBucket {
        gbBucket    :: Bucket
      , gbDelimiter :: Maybe T.Text
      , gbMarker    :: Maybe T.Text
      , gbMaxKeys   :: Maybe Int
      , gbPrefix    :: Maybe T.Text
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

data GetBucketResponse
    = GetBucketResponse {
        gbrName           :: Bucket
      , gbrDelimiter      :: Maybe T.Text
      , gbrMarker         :: Maybe T.Text
      , gbrMaxKeys        :: Maybe Int
      , gbrPrefix         :: Maybe T.Text
      , gbrContents       :: [ObjectInfo]
      , gbrCommonPrefixes :: [T.Text]
      }
    deriving (Show)

instance SignQuery GetBucket where
    type Info GetBucket = S3Info
    signQuery GetBucket {..} = s3SignQuery S3Query { 
                                 s3QMethod = Get
                               , s3QBucket = Just $ T.encodeUtf8 gbBucket
                               , s3QObject = Nothing
                               , s3QSubresources = []
                               , s3QQuery = HTTP.simpleQueryToQuery $ map (second T.encodeUtf8) $ catMaybes [
                                              ("delimiter",) <$> gbDelimiter
                                            , ("marker",) <$> gbMarker
                                            , ("max-keys",) . T.pack . show <$> gbMaxKeys
                                            , ("prefix",) <$> gbPrefix
                                            ]
                               , s3QAmzHeaders = []
                               , s3QRequestBody = Nothing
                               }

instance ResponseIteratee r GetBucketResponse where
    type ResponseMetadata GetBucketResponse = S3Metadata

    responseIteratee _ = s3XmlResponseIteratee parse
        where parse cursor
                  = do name <- force "Missing Name" $ cursor $/ elContent "Name"
                       let delimiter = listToMaybe $ cursor $/ elContent "Delimiter"
                       let marker = listToMaybe $ cursor $/ elContent "Marker"
                       maxKeys <- Data.Traversable.sequence . listToMaybe $ cursor $/ elContent "MaxKeys" &| textReadInt
                       let prefix = listToMaybe $ cursor $/ elContent "Prefix"
                       contents <- sequence $ cursor $/ Cu.laxElement "Contents" &| parseObjectInfo
                       let commonPrefixes = cursor $/ Cu.laxElement "CommonPrefixes" &// Cu.content
                       return GetBucketResponse{
                                                gbrName           = name
                                              , gbrDelimiter      = delimiter
                                              , gbrMarker         = marker
                                              , gbrMaxKeys        = maxKeys
                                              , gbrPrefix         = prefix
                                              , gbrContents       = contents
                                              , gbrCommonPrefixes = commonPrefixes
                                              }

instance Transaction GetBucket GetBucketResponse
