module Aws.S3.Commands.AbortMultipartUpload
       where

import           Aws.Core
import           Aws.S3.Core
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Types    as HTTP


data AbortMultipartUpload
  = AbortMultipartUpload {
    amuBucket :: Bucket
    , amuObjectName :: Object
    , amuUploadId :: T.Text
    }
  deriving (Show)

postAbortMultipartUpload :: Bucket -> T.Text -> T.Text -> AbortMultipartUpload
postAbortMultipartUpload b o i = AbortMultipartUpload b o i

data AbortMultipartUploadResponse
  = AbortMultipartUploadResponse {
    }

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery AbortMultipartUpload where
    type ServiceConfiguration AbortMultipartUpload = S3Configuration
    signQuery AbortMultipartUpload {..} = s3SignQuery S3Query {
      s3QMethod = Delete
      , s3QBucket = Just $ T.encodeUtf8 amuBucket
      , s3QObject = Just $ T.encodeUtf8 amuObjectName
      , s3QSubresources = HTTP.toQuery[
        ("uploadId" :: B8.ByteString, Just amuUploadId :: Maybe T.Text)
        ]
      , s3QQuery = []
      , s3QContentType = Nothing
      , s3QContentMd5 = Nothing
      , s3QAmzHeaders = []
      , s3QOtherHeaders = []
      , s3QRequestBody = Nothing
      }

instance ResponseConsumer r AbortMultipartUploadResponse where
    type ResponseMetadata AbortMultipartUploadResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse _cursor
                  = return AbortMultipartUploadResponse {}

instance Transaction AbortMultipartUpload AbortMultipartUploadResponse


instance AsMemoryResponse AbortMultipartUploadResponse where
    type MemoryResponse AbortMultipartUploadResponse = AbortMultipartUploadResponse
    loadToMemory = return
