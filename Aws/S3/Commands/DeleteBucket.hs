module Aws.S3.Commands.DeleteBucket
where

import           Aws.Core
import           Aws.S3.Core
import           Data.ByteString.Char8      ({- IsString -})
import qualified Data.Text.Encoding         as T

data DeleteBucket = DeleteBucket { dbBucket :: Bucket }
    deriving (Show)

data DeleteBucketResponse = DeleteBucketResponse {}
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery DeleteBucket where
    type ServiceConfiguration DeleteBucket = S3Configuration
    signQuery DeleteBucket {..} = s3SignQuery S3Query {
                                 s3QMethod = Delete
                               , s3QBucket = Just $ T.encodeUtf8 dbBucket
                               , s3QSubresources = []
                               , s3QQuery = []
                               , s3QContentType = Nothing
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = []
                               , s3QOtherHeaders = []
                               , s3QRequestBody = Nothing
                               , s3QObject = Nothing
                               }

instance ResponseConsumer DeleteBucket DeleteBucketResponse where
    type ResponseMetadata DeleteBucketResponse = S3Metadata
    responseConsumer _ _ = s3ResponseConsumer $ \_ -> return DeleteBucketResponse

instance Transaction DeleteBucket DeleteBucketResponse

instance AsMemoryResponse DeleteBucketResponse where
    type MemoryResponse DeleteBucketResponse = DeleteBucketResponse
    loadToMemory = return
