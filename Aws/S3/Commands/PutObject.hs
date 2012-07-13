module Aws.S3.Commands.PutObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow         (second)
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Maybe
import qualified Crypto.Hash.MD5       as MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive  as CI
import qualified Data.Conduit          as C
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP

data PutObject = PutObject {
  poObjectName :: T.Text,
  poBucket :: Bucket,
  poContentType :: Maybe B.ByteString,
  poCacheControl :: Maybe T.Text,
  poContentDisposition :: Maybe T.Text,
  poContentEncoding :: Maybe T.Text,
  poContentMD5 :: Maybe MD5.MD5,
  poExpires :: Maybe Int,
  poAcl :: Maybe CannedAcl,
  poStorageClass :: Maybe StorageClass,
  poRequestBody  :: HTTP.RequestBody (C.ResourceT IO),
  poMetadata :: [(T.Text,T.Text)]
}

putObject :: Bucket -> T.Text -> HTTP.RequestBody (C.ResourceT IO) -> PutObject
putObject bucket obj body = PutObject obj bucket Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing body []

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
                                            ] ++ map( \x -> (CI.mk . T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) poMetadata
                               , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes [
                                              ("Expires",) . T.pack . show <$> poExpires
                                            , ("Cache-Control",) <$> poCacheControl
                                            , ("Content-Disposition",) <$> poContentDisposition
                                            , ("Content-Encoding",) <$> poContentEncoding
                                            ]
                               , s3QRequestBody = Just poRequestBody
                               , s3QObject = Just $ T.encodeUtf8 poObjectName
                               }

instance ResponseConsumer PutObject PutObjectResponse where
    type ResponseMetadata PutObjectResponse = S3Metadata
    responseConsumer _ = s3ResponseConsumer $ \_status headers _body -> do
      let vid = T.decodeUtf8 `fmap` lookup "x-amz-version-id" headers
      return $ PutObjectResponse vid

instance Transaction PutObject PutObjectResponse

