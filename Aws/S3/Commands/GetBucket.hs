module Aws.S3.Commands.GetBucket
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import           Text.XML.Cursor       (($/), (&|), (&//))
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Traversable
import qualified Network.HTTP.Types    as HTTP
import qualified Text.XML.Cursor       as Cu

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
      , gbrIsTruncated    :: Bool
      }
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetBucket where
    type ServiceConfiguration GetBucket = S3Configuration
    signQuery GetBucket {..} = s3SignQuery S3Query {
                                 s3QMethod = Get
                               , s3QBucket = Just $ T.encodeUtf8 gbBucket
                               , s3QObject = Nothing
                               , s3QSubresources = []
                               , s3QQuery = HTTP.toQuery [
                                              ("delimiter" :: B8.ByteString ,) <$> gbDelimiter
                                            , ("marker",) <$> gbMarker
                                            , ("max-keys",) . T.pack . show <$> gbMaxKeys
                                            , ("prefix",) <$> gbPrefix
                                            ]
                               , s3QContentType = Nothing
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = []
                               , s3QOtherHeaders = []
                               , s3QRequestBody = Nothing
                               }

instance ResponseConsumer r GetBucketResponse where
    type ResponseMetadata GetBucketResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse cursor
                  = do name <- force "Missing Name" $ cursor $/ elContent "Name"
                       let delimiter = listToMaybe $ cursor $/ elContent "Delimiter"
                       let marker = listToMaybe $ cursor $/ elContent "Marker"
                       maxKeys <- Data.Traversable.sequence . listToMaybe $ cursor $/ elContent "MaxKeys" &| textReadInt
                       let truncated = maybe True (/= "false") $ listToMaybe $ cursor $/ elContent "IsTruncated"
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
                                              , gbrIsTruncated    = truncated
                                              }

instance Transaction GetBucket GetBucketResponse

instance AsMemoryResponse GetBucketResponse where
    type MemoryResponse GetBucketResponse = GetBucketResponse
    loadToMemory = return