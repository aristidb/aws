module Aws.S3.Commands.GetBucketObjectVersions
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
import           Prelude
import qualified Network.HTTP.Types    as HTTP
import qualified Text.XML.Cursor       as Cu
import qualified Text.XML              as XML

data GetBucketObjectVersions
    = GetBucketObjectVersions {
        gbvBucket          :: Bucket
      , gbvDelimiter       :: Maybe T.Text
      , gbvKeyMarker       :: Maybe T.Text
      , gbvMaxKeys         :: Maybe Int
      , gbvPrefix          :: Maybe T.Text
      , gbvVersionIdMarker :: Maybe T.Text
      }
    deriving (Show)

getBucketObjectVersions :: Bucket -> GetBucketObjectVersions
getBucketObjectVersions bucket
    = GetBucketObjectVersions {
        gbvBucket          = bucket
      , gbvDelimiter       = Nothing
      , gbvKeyMarker       = Nothing
      , gbvMaxKeys         = Nothing
      , gbvPrefix          = Nothing
      , gbvVersionIdMarker = Nothing
      }

data GetBucketObjectVersionsResponse
    = GetBucketObjectVersionsResponse {
        gbvrName                :: Bucket
      , gbvrDelimiter           :: Maybe T.Text
      , gbvrKeyMarker           :: Maybe T.Text
      , gbvrMaxKeys             :: Maybe Int
      , gbvrPrefix              :: Maybe T.Text
      , gbvrVersionIdMarker     :: Maybe T.Text
      , gbvrContents            :: [ObjectVersionInfo]
      , gbvrCommonPrefixes      :: [T.Text]
      , gbvrIsTruncated         :: Bool
      , gbvrNextKeyMarker       :: Maybe T.Text
      , gbvrNextVersionIdMarker :: Maybe T.Text
      }
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetBucketObjectVersions where
    type ServiceConfiguration GetBucketObjectVersions = S3Configuration
    signQuery GetBucketObjectVersions {..} = s3SignQuery S3Query {
                                 s3QMethod = Get
                               , s3QBucket = Just $ T.encodeUtf8 gbvBucket
                               , s3QObject = Nothing
                               , s3QSubresources = [ ("versions", Nothing) ]
                               , s3QQuery = HTTP.toQuery [
                                              ("delimiter" :: B8.ByteString ,) <$> gbvDelimiter
                                            , ("key-marker",) <$> gbvKeyMarker
                                            , ("max-keys",) . T.pack . show <$> gbvMaxKeys
                                            , ("prefix",) <$> gbvPrefix
                                            , ("version-id-marker",) <$> gbvVersionIdMarker
                                            ]
                               , s3QContentType = Nothing
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = []
                               , s3QOtherHeaders = []
                               , s3QRequestBody = Nothing
                               }

instance ResponseConsumer r GetBucketObjectVersionsResponse where
    type ResponseMetadata GetBucketObjectVersionsResponse = S3Metadata

    responseConsumer _ _ = s3XmlResponseConsumer parse
        where parse cursor
                  = do name <- force "Missing Name" $ cursor $/ elContent "Name"
                       let delimiter = listToMaybe $ cursor $/ elContent "Delimiter"
                       let keyMarker = listToMaybe $ cursor $/ elContent "KeyMarker"
                       let versionMarker = listToMaybe $ cursor $/ elContent "VersionIdMarker"
                       maxKeys <- Data.Traversable.sequence . listToMaybe $ cursor $/ elContent "MaxKeys" &| textReadInt
                       let truncated = maybe True (/= "false") $ listToMaybe $ cursor $/ elContent "IsTruncated"
                       let nextKeyMarker = listToMaybe $ cursor $/ elContent "NextKeyMarker"
                       let nextVersionMarker = listToMaybe $ cursor $/ elContent "NextVersionIdMarker"
                       let prefix = listToMaybe $ cursor $/ elContent "Prefix"
                       contents <- sequence $ cursor $/ Cu.checkName objectNodeName &| parseObjectVersionInfo
                       let commonPrefixes = cursor $/ Cu.laxElement "CommonPrefixes" &// Cu.content
                       return GetBucketObjectVersionsResponse{
                                                gbvrName                = name
                                              , gbvrDelimiter           = delimiter
                                              , gbvrKeyMarker           = keyMarker
                                              , gbvrMaxKeys             = maxKeys
                                              , gbvrPrefix              = prefix
                                              , gbvrVersionIdMarker     = versionMarker
                                              , gbvrContents            = contents
                                              , gbvrCommonPrefixes      = commonPrefixes
                                              , gbvrIsTruncated         = truncated
                                              , gbvrNextKeyMarker       = nextKeyMarker
                                              , gbvrNextVersionIdMarker = nextVersionMarker
                                              }
              objectNodeName n = let fn = T.toCaseFold $ XML.nameLocalName n
                                  in fn == T.toCaseFold "Version" || fn == T.toCaseFold "DeleteMarker"

instance Transaction GetBucketObjectVersions GetBucketObjectVersionsResponse

instance IteratedTransaction GetBucketObjectVersions GetBucketObjectVersionsResponse where
    nextIteratedRequest request response
        = case (gbvrIsTruncated response, gbvrNextKeyMarker response, gbvrNextVersionIdMarker response, gbvrContents response) of
            (True, Just keyMarker, Just versionMarker, _             ) -> Just $ request { gbvKeyMarker = Just keyMarker, gbvVersionIdMarker = Just versionMarker }
            (True, Nothing,        Nothing,            contents@(_:_)) -> Just $ request { gbvKeyMarker = Just $ oviKey $ last contents, gbvVersionIdMarker = Just $ oviVersionId $ last contents }
            (_,    _,              _,                  _             ) -> Nothing

instance ListResponse GetBucketObjectVersionsResponse ObjectVersionInfo where
    listResponse = gbvrContents

instance AsMemoryResponse GetBucketObjectVersionsResponse where
    type MemoryResponse GetBucketObjectVersionsResponse = GetBucketObjectVersionsResponse
    loadToMemory = return
