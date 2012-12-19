module Aws.S3.Commands.HeadObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types    as HTTP

data HeadObject
    = HeadObject {
        hoBucket :: Bucket
      , hoObjectName :: Object
      , hoVersionId :: Maybe T.Text
      }
  deriving (Show)

headObject :: Bucket -> T.Text -> HeadObject
headObject b o = HeadObject b o Nothing

data HeadObjectResponse
    = HeadObjectResponse {
        horMetadata :: ObjectMetadata
      }

data HeadObjectMemoryResponse
    = HeadObjectMemoryResponse ObjectMetadata
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery HeadObject where
    type ServiceConfiguration HeadObject = S3Configuration
    signQuery HeadObject {..} = s3SignQuery S3Query {
                                   s3QMethod = Head
                                 , s3QBucket = Just $ T.encodeUtf8 hoBucket
                                 , s3QObject = Just $ T.encodeUtf8 hoObjectName
                                 , s3QSubresources = HTTP.toQuery [
                                                       ("versionId" :: B8.ByteString,) <$> hoVersionId
                                                     ]
                                 , s3QQuery = []
                                 , s3QContentType = Nothing
                                 , s3QContentMd5 = Nothing
                                 , s3QAmzHeaders = []
                                 , s3QOtherHeaders = []
                                 , s3QRequestBody = Nothing
                                 }

instance ResponseConsumer HeadObject HeadObjectResponse where
    type ResponseMetadata HeadObjectResponse = S3Metadata
    responseConsumer HeadObject{..} _ resp
        = HeadObjectResponse <$> parseObjectMetadata (HTTP.responseHeaders resp)

instance Transaction HeadObject HeadObjectResponse

instance AsMemoryResponse HeadObjectResponse where
    type MemoryResponse HeadObjectResponse = HeadObjectMemoryResponse
    loadToMemory (HeadObjectResponse om) = return (HeadObjectMemoryResponse om)
