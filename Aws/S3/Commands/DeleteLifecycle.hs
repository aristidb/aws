{-# LANGUAGE TypeFamilies, RecordWildCards, TupleSections, OverloadedStrings, MultiParamTypeClasses #-}
module Aws.S3.Commands.DeleteLifecycle
where

import           Aws.Core
import           Aws.S3.Core
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Types    as HTTP

data DeleteLifecycle
    = DeleteLifecycle {
        dlBucket :: Bucket
      }
    deriving (Show)

data DeleteLifecycleResponse = DeleteLifecycleResponse deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery DeleteLifecycle where
    type ServiceConfiguration DeleteLifecycle = S3Configuration
    signQuery DeleteLifecycle {..} = s3SignQuery S3Query {
                                       s3QMethod = Delete
                                     , s3QBucket = Just $ T.encodeUtf8 dlBucket
                                     , s3QObject = Nothing
                                     , s3QSubresources = HTTP.toQuery [ ( "lifecycle" :: B8.ByteString
                                                                        , Nothing :: Maybe B8.ByteString ) ]
                                     , s3QQuery = []
                                     , s3QContentType = Nothing
                                     , s3QContentMd5 = Nothing
                                     , s3QAmzHeaders = []
                                     , s3QOtherHeaders = []
                                     , s3QRequestBody = Nothing
                                     }

instance ResponseConsumer DeleteLifecycle DeleteLifecycleResponse where
    type ResponseMetadata DeleteLifecycleResponse = S3Metadata
    responseConsumer _ = s3ResponseConsumer $ \_ _ _ ->
                         return DeleteLifecycleResponse


instance Transaction DeleteLifecycle DeleteLifecycleResponse

