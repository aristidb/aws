module Aws.S3.Commands.GetObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Monad.Trans.Resource (ResourceT, throwM)
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as L
import qualified Data.Conduit          as C
import qualified Data.Conduit.List     as CL
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
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
      , goIfMatch :: Maybe T.Text
      -- ^ Return the object only if its entity tag (ETag, which is an md5sum of the content) is the same as the one specified; otherwise, catch a 'StatusCodeException' with a status of 412 precondition failed.
      , goIfNoneMatch :: Maybe T.Text
      -- ^ Return the object only if its entity tag (ETag, which is an md5sum of the content) is different from the one specified; otherwise, catch a 'StatusCodeException' with a status of 304 not modified.
      }
  deriving (Show)

getObject :: Bucket -> T.Text -> GetObject
getObject b o = GetObject b o Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data GetObjectResponse
    = GetObjectResponse {
        gorMetadata :: ObjectMetadata,
        gorResponse :: HTTP.Response (C.ResumableSource (ResourceT IO) B8.ByteString)
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
                                 , s3QContentSha256 = Nothing
                                 , s3QAmzHeaders = []
                                 , s3QOtherHeaders = catMaybes [
                                                       decodeRange <$> goResponseContentRange
                                                     , ("if-match",) . T.encodeUtf8 <$> goIfMatch
                                                     , ("if-none-match",) . T.encodeUtf8 <$> goIfNoneMatch
                                                     ]
                                 , s3QRequestBody = Nothing
                                 }
      where decodeRange (pos,len) = ("range",B8.concat $ ["bytes=", B8.pack (show pos), "-", B8.pack (show len)])

instance ResponseConsumer GetObject GetObjectResponse where
    type ResponseMetadata GetObjectResponse = S3Metadata
    responseConsumer GetObject{..} metadata resp
        | status == HTTP.status200 = do
            rsp <- s3BinaryResponseConsumer return metadata resp
            om <- parseObjectMetadata (HTTP.responseHeaders resp)
            return $ GetObjectResponse om rsp
        | otherwise = throwM $ HTTP.StatusCodeException status headers cookies
      where
        status  = HTTP.responseStatus    resp
        headers = HTTP.responseHeaders   resp
        cookies = HTTP.responseCookieJar resp

instance Transaction GetObject GetObjectResponse

instance AsMemoryResponse GetObjectResponse where
    type MemoryResponse GetObjectResponse = GetObjectMemoryResponse
    loadToMemory (GetObjectResponse om x) = do
        bss <- HTTP.responseBody x C.$$+- CL.consume
        return $ GetObjectMemoryResponse om x
            { HTTP.responseBody = L.fromChunks bss
            }
