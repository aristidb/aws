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
import qualified Data.ByteString.UTF8       as BU
import qualified Data.Text                  as T
import qualified Data.Traversable
import qualified Network.HTTP.Types         as HTTP
import qualified Text.XML.Enumerator.Cursor as Cu

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

data GetBucketResponse
    = GetBucketResponse {
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
                                 s3QMethod = Get
                               , s3QBucket = Just $ BU.fromString gbBucket
                               , s3QSubresources = []
                               , s3QQuery = HTTP.simpleQueryToQuery $ map (second BU.fromString) $ catMaybes [
                                              ("delimiter",) <$> gbDelimiter
                                            , ("marker",) <$> gbMarker
                                            , ("max-keys",) <$> show <$> gbMaxKeys
                                            , ("prefix",) <$> gbPrefix
                                            ]
                               , s3QRequestBody = Nothing
                               }

instance ResponseIteratee GetBucketResponse where
    type ResponseMetadata GetBucketResponse = S3Metadata

    responseIteratee = s3XmlResponseIteratee parse
        where parse cursor
                  = do name <- force "Missing Name" $ cursor $/ elCont "Name"
                       let delimiter = listToMaybe $ cursor $/ elCont "Delimiter"
                       let marker = listToMaybe $ cursor $/ elCont "Marker"
                       maxKeys <- Data.Traversable.sequence . listToMaybe $ cursor $/ elCont "MaxKeys" &| readInt
                       let prefix = listToMaybe $ cursor $/ elCont "Prefix"
                       contents <- sequence $ cursor $/ Cu.laxElement "Contents" &| parseObjectInfo
                       let commonPrefixes = cursor $/ Cu.laxElement "CommonPrefixes" &// Cu.content &| T.unpack
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
