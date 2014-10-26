module Aws.S3.Commands.GetObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Client   as HTTP
import qualified Network.HTTP.Types    as HTTP

data GetObject
    = GetObject {
        goBucket :: Bucket
      , goObjectName :: Object
      , goVersionId :: Maybe T.Text
      , goResponseContentType :: Maybe T.Text
      , goResponseContentLanguage :: Maybe T.Text
      , goResponseExpires :: Maybe T.Text
      , goResponseCacheControl :: Maybe T.Text
      , goResponseContentDisposition :: Maybe T.Text
      , goResponseContentEncoding :: Maybe T.Text
      , goResponseContentRange :: Maybe (Int,Int)
      }
  deriving (Show)

getObject :: Bucket -> T.Text -> GetObject
getObject b o = GetObject b o Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data GetObjectResponse
    = GetObjectResponse {
        gorMetadata :: ObjectMetadata,
        gorResponse :: HTTP.Response HTTP.BodyReader
      }

data GetObjectMemoryResponse
    = GetObjectMemoryResponse ObjectMetadata (HTTP.Response L.ByteString)
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetObject where
    type ServiceConfiguration GetObject = S3Configuration
    signQuery GetObject {..} = s3SignQuery S3Query {
                                   s3QMethod = Get
                                 , s3QBucket = Just $ T.encodeUtf8 goBucket
                                 , s3QObject = Just $ T.encodeUtf8 goObjectName
                                 , s3QSubresources = HTTP.toQuery [
                                                       ("versionId" :: B8.ByteString,) <$> goVersionId
                                                     , ("response-content-type" :: B8.ByteString,) <$> goResponseContentType
                                                     , ("response-content-language",) <$> goResponseContentLanguage
                                                     , ("response-expires",) <$> goResponseExpires
                                                     , ("response-cache-control",) <$> goResponseCacheControl
                                                     , ("response-content-disposition",) <$> goResponseContentDisposition
                                                     , ("response-content-encoding",) <$> goResponseContentEncoding
                                                     ]
                                 , s3QQuery = []
                                 , s3QContentType = Nothing
                                 , s3QContentMd5 = Nothing
                                 , s3QAmzHeaders = []
                                 , s3QOtherHeaders = catMaybes [
                                                       decodeRange <$> goResponseContentRange
                                                     ]
                                 , s3QRequestBody = Nothing
                                 }
      where decodeRange (pos,len) = ("range",B8.concat $ ["bytes=", B8.pack (show pos), "-", B8.pack (show len)])

instance ResponseConsumer GetObject GetObjectResponse where
    type ResponseMetadata GetObjectResponse = S3Metadata
    responseConsumer GetObject{..} metadata resp
        = do rsp <- s3BinaryResponseConsumer return metadata resp
             om <- parseObjectMetadata (HTTP.responseHeaders resp)
             return $ GetObjectResponse om rsp

instance Transaction GetObject GetObjectResponse

instance AsMemoryResponse GetObjectResponse where
    type MemoryResponse GetObjectResponse = GetObjectMemoryResponse
    loadToMemory (GetObjectResponse om x) = do
        bss <- HTTP.brConsume $ HTTP.responseBody x
        return $ GetObjectMemoryResponse om x
            { HTTP.responseBody = L.fromChunks bss
            }
