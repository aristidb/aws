module Aws.S3.Commands.DeleteObjectVersion
where

import           Aws.Core
import           Aws.S3.Core
import           Data.ByteString.Char8      ({- IsString -})
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

data DeleteObjectVersion = DeleteObjectVersion {
  dovObjectName :: T.Text,
  dovBucket :: Bucket,
  dovVersionId :: T.Text
}

data DeleteObjectVersionResponse = DeleteObjectVersionResponse {
}

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery DeleteObjectVersion where
    type ServiceConfiguration DeleteObjectVersion = S3Configuration
    signQuery DeleteObjectVersion {..} = s3SignQuery S3Query {
                                 s3QMethod = Delete
                               , s3QBucket = Just $ T.encodeUtf8 dovBucket
                               , s3QSubresources = [ ("versionId", Just $ T.encodeUtf8 dovVersionId) ]
                               , s3QQuery = []
                               , s3QContentType = Nothing
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = []
                               , s3QOtherHeaders = []
                               , s3QRequestBody = Nothing
                               , s3QObject = Just $ T.encodeUtf8 dovObjectName
                               }

instance ResponseConsumer DeleteObjectVersion DeleteObjectVersionResponse where
    type ResponseMetadata DeleteObjectVersionResponse = S3Metadata
    responseConsumer _ _
        = s3ResponseConsumer $ \_ -> return DeleteObjectVersionResponse

instance Transaction DeleteObjectVersion DeleteObjectVersionResponse

instance AsMemoryResponse DeleteObjectVersionResponse where
    type MemoryResponse DeleteObjectVersionResponse = DeleteObjectVersionResponse
    loadToMemory = return
