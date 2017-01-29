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
        gbovBucket          :: Bucket
      , gbovDelimiter       :: Maybe T.Text
      , gbovKeyMarker       :: Maybe T.Text
      , gbovMaxKeys         :: Maybe Int
      , gbovPrefix          :: Maybe T.Text
      , gbovVersionIdMarker :: Maybe T.Text
      }
    deriving (Show)

getBucketObjectVersions :: Bucket -> GetBucketObjectVersions
getBucketObjectVersions bucket
    = GetBucketObjectVersions {
        gbovBucket          = bucket
      , gbovDelimiter       = Nothing
      , gbovKeyMarker       = Nothing
      , gbovMaxKeys         = Nothing
      , gbovPrefix          = Nothing
      , gbovVersionIdMarker = Nothing
      }

data GetBucketObjectVersionsResponse
    = GetBucketObjectVersionsResponse {
        gbovrName                :: Bucket
      , gbovrDelimiter           :: Maybe T.Text
      , gbovrKeyMarker           :: Maybe T.Text
      , gbovrMaxKeys             :: Maybe Int
      , gbovrPrefix              :: Maybe T.Text
      , gbovrVersionIdMarker     :: Maybe T.Text
      , gbovrContents            :: [ObjectVersionInfo]
      , gbovrCommonPrefixes      :: [T.Text]
      , gbovrIsTruncated         :: Bool
      , gbovrNextKeyMarker       :: Maybe T.Text
      , gbovrNextVersionIdMarker :: Maybe T.Text
      }
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetBucketObjectVersions where
    type ServiceConfiguration GetBucketObjectVersions = S3Configuration
    signQuery GetBucketObjectVersions {..} = s3SignQuery S3Query {
                                 s3QMethod = Get
                               , s3QBucket = Just $ T.encodeUtf8 gbovBucket
                               , s3QObject = Nothing
                               , s3QSubresources = [ ("versions", Nothing) ]
                               , s3QQuery = HTTP.toQuery [
                                              ("delimiter" :: B8.ByteString ,) <$> gbovDelimiter
                                            , ("key-marker",) <$> gbovKeyMarker
                                            , ("max-keys",) . T.pack . show <$> gbovMaxKeys
                                            , ("prefix",) <$> gbovPrefix
                                            , ("version-id-marker",) <$> gbovVersionIdMarker
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
                                                gbovrName                = name
                                              , gbovrDelimiter           = delimiter
                                              , gbovrKeyMarker           = keyMarker
                                              , gbovrMaxKeys             = maxKeys
                                              , gbovrPrefix              = prefix
                                              , gbovrVersionIdMarker     = versionMarker
                                              , gbovrContents            = contents
                                              , gbovrCommonPrefixes      = commonPrefixes
                                              , gbovrIsTruncated         = truncated
                                              , gbovrNextKeyMarker       = nextKeyMarker
                                              , gbovrNextVersionIdMarker = nextVersionMarker
                                              }
              objectNodeName n = let fn = T.toCaseFold $ XML.nameLocalName n
                                  in fn == T.toCaseFold "Version" || fn == T.toCaseFold "DeleteMarker"

instance Transaction GetBucketObjectVersions GetBucketObjectVersionsResponse

instance IteratedTransaction GetBucketObjectVersions GetBucketObjectVersionsResponse where
    nextIteratedRequest request response
        = case (gbovrIsTruncated response, gbovrNextKeyMarker response, gbovrNextVersionIdMarker response, gbovrContents response) of
            (True, Just keyMarker, Just versionMarker, _             ) -> Just $ request { gbovKeyMarker = Just keyMarker, gbovVersionIdMarker = Just versionMarker }
            (True, Nothing,        Nothing,            contents@(_:_)) -> Just $ request { gbovKeyMarker = Just $ oviKey $ last contents, gbovVersionIdMarker = Just $ oviVersionId $ last contents }
            (_,    _,              _,                  _             ) -> Nothing

instance ListResponse GetBucketObjectVersionsResponse ObjectVersionInfo where
    listResponse = gbovrContents

instance AsMemoryResponse GetBucketObjectVersionsResponse where
    type MemoryResponse GetBucketObjectVersionsResponse = GetBucketObjectVersionsResponse
    loadToMemory = return
