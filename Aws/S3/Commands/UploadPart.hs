{-# LANGUAGE CPP #-}
module Aws.S3.Commands.UploadPart
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow         (second)
import           Crypto.Hash
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.CaseInsensitive  as CI
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types  as HTTP

data UploadPart = UploadPart {
  upObjectName :: T.Text,
  upBucket :: Bucket,
  upPartNumber :: Integer,
  upUploadId :: T.Text,
  upContentType :: Maybe B.ByteString,
  upCacheControl :: Maybe T.Text,
  upContentDisposition :: Maybe T.Text,
  upContentEncoding :: Maybe T.Text,
  upContentMD5 :: Maybe (Digest MD5),
  upExpires :: Maybe Int,
  upAcl :: Maybe CannedAcl,
  upStorageClass :: Maybe StorageClass,
  upWebsiteRedirectLocation :: Maybe T.Text,
  upServerSideEncryption :: Maybe ServerSideEncryption,
#if MIN_VERSION_http_conduit(2, 0, 0)
  upRequestBody  :: HTTP.RequestBody,
#else
  upRequestBody  :: HTTP.RequestBody (C.ResourceT IO),
#endif
  upMetadata :: [(T.Text,T.Text)],
  upAutoMakeBucket :: Bool -- ^ Internet Archive S3 nonstandard extension
}

#if MIN_VERSION_http_conduit(2, 0, 0)
uploadPart :: Bucket -> T.Text -> Integer -> T.Text -> HTTP.RequestBody -> UploadPart
#else
uploadPart :: Bucket -> T.Text -> Integer -> T.Text -> HTTP.RequestBody (C.ResourceT IO) -> UploadPart
#endif
uploadPart bucket obj p i body = UploadPart obj bucket p i Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing body [] False

data UploadPartResponse
  = UploadPartResponse {
      uprVersionId :: Maybe T.Text
    }
  deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery UploadPart where
    type ServiceConfiguration UploadPart = S3Configuration
    signQuery UploadPart {..} = s3SignQuery S3Query {
                                 s3QMethod = Put
                               , s3QBucket = Just $ T.encodeUtf8 upBucket
                               , s3QObject = Just $ T.encodeUtf8 upObjectName
                               , s3QSubresources = HTTP.toQuery[
                                   ("partNumber" :: B8.ByteString , Just (T.pack (show upPartNumber)) :: Maybe T.Text)
                                 , ("uploadId" :: B8.ByteString, Just upUploadId :: Maybe T.Text)
                                 ]
                               , s3QQuery = []
                               , s3QContentType = upContentType
                               , s3QContentMd5 = upContentMD5
                               , s3QAmzHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("x-amz-acl",) <$> writeCannedAcl <$> upAcl
                                            , ("x-amz-storage-class",) <$> writeStorageClass <$> upStorageClass
                                            , ("x-amz-website-redirect-location",) <$> upWebsiteRedirectLocation
                                            , ("x-amz-server-side-encryption",) <$> writeServerSideEncryption <$> upServerSideEncryption
					    , if upAutoMakeBucket then Just ("x-amz-auto-make-bucket", "1")  else Nothing
                                            ] ++ map( \x -> (CI.mk . T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) upMetadata
                               , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("Expires",) . T.pack . show <$> upExpires
                                            , ("Cache-Control",) <$> upCacheControl
                                            , ("Content-Disposition",) <$> upContentDisposition
                                            , ("Content-Encoding",) <$> upContentEncoding
                                            ]
                               , s3QRequestBody = Just upRequestBody
                               }

instance ResponseConsumer UploadPart UploadPartResponse where
    type ResponseMetadata UploadPartResponse = S3Metadata
    responseConsumer _ = s3ResponseConsumer $ \resp -> do
      let vid = T.decodeUtf8 `fmap` lookup "x-amz-version-id" (HTTP.responseHeaders resp)
      return $ UploadPartResponse vid

instance Transaction UploadPart UploadPartResponse

instance AsMemoryResponse UploadPartResponse where
    type MemoryResponse UploadPartResponse = UploadPartResponse
    loadToMemory = return
