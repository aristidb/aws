module Aws.S3.Commands.InitiateMultipartUpload
       where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow         (second)
import           Data.ByteString.Char8 ({- IsString -})
import           Text.XML.Cursor       (($/))
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Types    as HTTP


data InitiateMultipartUpload
  = InitiateMultipartUpload {
    imuBucket :: Bucket
    , imuObjectName :: Object
    , imuCacheControl :: Maybe T.Text
    , imuContentDisposition :: Maybe T.Text
    , imuContentEncoding :: Maybe T.Text
    , imuContentType :: Maybe T.Text
    , imuExpires :: Maybe Int
    , imuXAmzMeta :: Maybe T.Text
    , imuXAmzStorageClass :: Maybe T.Text
    , imuXAmzWebsiteRedirectLocation :: Maybe T.Text
    }
  deriving (Show)

postInitiateMultipartUpload :: Bucket -> T.Text -> InitiateMultipartUpload
postInitiateMultipartUpload b o = InitiateMultipartUpload b o Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data InitiateMultipartUploadResponse
  = InitiateMultipartUploadResponse {
      imurBucket   :: Bucket
    , imurKey      :: T.Text
    , imurUploadId :: T.Text
    }

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery InitiateMultipartUpload where
    type ServiceConfiguration InitiateMultipartUpload = S3Configuration
    signQuery InitiateMultipartUpload {..} = s3SignQuery S3Query {
      s3QMethod = Post
      , s3QBucket = Just $ T.encodeUtf8 imuBucket
      , s3QObject = Just $ T.encodeUtf8 $ imuObjectName
      , s3QSubresources = HTTP.toQuery[ ("uploads" :: B8.ByteString , Nothing :: Maybe B8.ByteString)]
      , s3QQuery = []
      , s3QContentType = T.encodeUtf8 <$> imuContentType
      , s3QContentMd5 = Nothing
      , s3QAmzHeaders = catMaybes [ ("x-amz-meta",) <$> (T.encodeUtf8 <$> imuXAmzMeta)
                                  , ("x-amz-storage-class",) <$> (T.encodeUtf8 <$> imuXAmzStorageClass)
                                  , ("x-amz-website-redirect-location",) <$> (T.encodeUtf8 <$> imuXAmzWebsiteRedirectLocation)
                                  ]
      , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes [
          ("Expires",) . T.pack . show <$> imuExpires
        , ("Cache-Control",) <$> imuCacheControl
        , ("Content-Disposition",) <$> imuContentDisposition
        , ("Content-Encoding",) <$> imuContentEncoding
        ]
      , s3QRequestBody = Nothing
      }

instance ResponseConsumer r InitiateMultipartUploadResponse where
    type ResponseMetadata InitiateMultipartUploadResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse cursor
                  = do bucket <- force "Missing Bucket Name" $ cursor $/ elContent "Bucket"
                       key <- force "Missing Key" $ cursor $/ elContent "Key"
                       uploadId <- force "Missing UploadID" $ cursor $/ elContent "UploadId"
                       return InitiateMultipartUploadResponse{
                                                imurBucket         = bucket
                                              , imurKey            = key
                                              , imurUploadId       = uploadId
                                              }

instance Transaction InitiateMultipartUpload InitiateMultipartUploadResponse

instance AsMemoryResponse InitiateMultipartUploadResponse where
    type MemoryResponse InitiateMultipartUploadResponse = InitiateMultipartUploadResponse
    loadToMemory = return
