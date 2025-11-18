{-# LANGUAGE CPP #-}
module Aws.S3.Commands.PutObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow          (second)
import qualified Crypto.Hash            as CH
import           Data.ByteString.Char8  ({- IsString -})
import           Data.Maybe
import qualified Data.ByteString.Char8  as B
import qualified Data.CaseInsensitive   as CI
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Prelude
import qualified Network.HTTP.Conduit   as HTTP
import qualified Network.HTTP.Types.URI as URI

data PutObject = PutObject {
  poObjectName :: T.Text,
  poBucket :: Bucket,
  poContentType :: Maybe B.ByteString,
  poCacheControl :: Maybe T.Text,
  poContentDisposition :: Maybe T.Text,
  poContentEncoding :: Maybe T.Text,
  poContentMD5 :: Maybe (CH.Digest CH.MD5),
  poExpires :: Maybe Int,
  poAcl :: Maybe CannedAcl,
  poStorageClass :: Maybe StorageClass,
  poWebsiteRedirectLocation :: Maybe T.Text,
  poServerSideEncryption :: Maybe ServerSideEncryption,
  poRequestBody  :: HTTP.RequestBody,
  poMetadata :: [(T.Text,T.Text)],
  poAutoMakeBucket :: Bool, -- ^ Internet Archive S3 nonstandard extension
  poExpect100Continue :: Bool, -- ^ Note: Requires http-client >= 0.4.10
  poTagging :: [(T.Text,T.Text)] -- ^ tag-set as key/value pairs
}

putObject :: Bucket -> T.Text -> HTTP.RequestBody -> PutObject
putObject bucket obj body = PutObject obj bucket Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing body [] False False []

data PutObjectResponse
  = PutObjectResponse
      { porVersionId :: Maybe T.Text
      , porETag :: T.Text
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
                               , s3QAmzHeaders = map (second T.encodeUtf8) (catMaybes [
                                              ("x-amz-acl",) <$> writeCannedAcl <$> poAcl
                                            , ("x-amz-storage-class",) <$> writeStorageClass <$> poStorageClass
                                            , ("x-amz-website-redirect-location",) <$> poWebsiteRedirectLocation
                                            , ("x-amz-server-side-encryption",) <$> writeServerSideEncryption <$> poServerSideEncryption
                                            , if poAutoMakeBucket then Just ("x-amz-auto-make-bucket", "1")  else Nothing
                                            ] ++ map( \x -> (CI.mk . T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) poMetadata
                                            ) ++ if null poTagging
                                                then []
                                                else [("x-amz-tagging", URI.renderQuery False $ URI.queryTextToQuery $ map (second Just) poTagging)]
                               , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("Expires",) . T.pack . show <$> poExpires
                                            , ("Cache-Control",) <$> poCacheControl
                                            , ("Content-Disposition",) <$> poContentDisposition
                                            , ("Content-Encoding",) <$> poContentEncoding
                                            , if poExpect100Continue
                                                  then Just ("Expect", "100-continue")
                                                  else Nothing
                                            ]
                               , s3QRequestBody = Just poRequestBody
                               , s3QObject = Just $ T.encodeUtf8 poObjectName
                               }

instance ResponseConsumer PutObject PutObjectResponse where
    type ResponseMetadata PutObjectResponse = S3Metadata
    responseConsumer _ _ = s3ResponseConsumer $ \resp -> do
      let vid = T.decodeUtf8 `fmap` lookup "x-amz-version-id" (HTTP.responseHeaders resp)
      let etag = fromMaybe "" $ T.decodeUtf8 `fmap` lookup "ETag" (HTTP.responseHeaders resp)
      return $ PutObjectResponse vid etag

instance Transaction PutObject PutObjectResponse

instance AsMemoryResponse PutObjectResponse where
    type MemoryResponse PutObjectResponse = PutObjectResponse
    loadToMemory = return
