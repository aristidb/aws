{-# LANGUAGE CPP #-}
module Aws.S3.Commands.PutObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow         (second)
import           Crypto.Hash
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive  as CI
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types    as HTTP

data PutObject = PutObject {
  poObjectName :: T.Text,
  poBucket :: Bucket,
  poContentType :: Maybe B.ByteString,
  poCacheControl :: Maybe T.Text,
  poContentDisposition :: Maybe T.Text,
  poContentEncoding :: Maybe T.Text,
  poContentMD5 :: Maybe (Digest MD5),
  poExpires :: Maybe Int,
  poAcl :: Maybe CannedAcl,
  poStorageClass :: Maybe StorageClass,
  poWebsiteRedirectLocation :: Maybe T.Text,
  poServerSideEncryption :: Maybe ServerSideEncryption,
#if MIN_VERSION_http_conduit(2, 0, 0)
  poRequestBody  :: HTTP.RequestBody,
#else
  poRequestBody  :: HTTP.RequestBody (C.ResourceT IO),
#endif
  poMetadata :: [(T.Text,T.Text)],
  poAutoMakeBucket :: Bool -- ^ Internet Archive S3 nonstandard extension
}

#if MIN_VERSION_http_conduit(2, 0, 0)
putObject :: Bucket -> T.Text -> HTTP.RequestBody -> PutObject
#else
putObject :: Bucket -> T.Text -> HTTP.RequestBody (C.ResourceT IO) -> PutObject
#endif
putObject bucket obj body = PutObject obj bucket Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing body [] False

data PutObjectResponse
  = PutObjectResponse {
      porVersionId :: Maybe T.Text
    }
  deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery PutObject where
    type ServiceConfiguration PutObject = S3Configuration
    signQuery PutObject {..} = s3SignQuery S3Query {
                                 s3QMethod = Put
                               , s3QBucket = Just $ T.encodeUtf8 poBucket
                               , s3QSubresources = []
                               , s3QQuery = []
                               , s3QContentType = poContentType
                               , s3QContentMd5 = poContentMD5
                               , s3QAmzHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("x-amz-acl",) <$> writeCannedAcl <$> poAcl
                                            , ("x-amz-storage-class",) <$> writeStorageClass <$> poStorageClass
                                            , ("x-amz-website-redirect-location",) <$> poWebsiteRedirectLocation
                                            , ("x-amz-server-side-encryption",) <$> writeServerSideEncryption <$> poServerSideEncryption
					    , if poAutoMakeBucket then Just ("x-amz-auto-make-bucket", "1")  else Nothing
                                            ] ++ map( \x -> (CI.mk . T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) poMetadata
                               , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("Expires",) . T.pack . show <$> poExpires
                                            , ("Cache-Control",) <$> poCacheControl
                                            , ("Content-Disposition",) <$> poContentDisposition
                                            , ("Content-Encoding",) <$> poContentEncoding
                                            ]
                               , s3QRequestBody = Just poRequestBody
                               , s3QObject = Just . HTTP.urlEncode False $ T.encodeUtf8 poObjectName
                               }

instance ResponseConsumer PutObject PutObjectResponse where
    type ResponseMetadata PutObjectResponse = S3Metadata
    responseConsumer _ = s3ResponseConsumer $ \resp -> do
      let vid = T.decodeUtf8 `fmap` lookup "x-amz-version-id" (HTTP.responseHeaders resp)
      return $ PutObjectResponse vid

instance Transaction PutObject PutObjectResponse

instance AsMemoryResponse PutObjectResponse where
    type MemoryResponse PutObjectResponse = PutObjectResponse
    loadToMemory = return
