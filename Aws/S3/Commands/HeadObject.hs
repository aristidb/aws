module Aws.S3.Commands.HeadObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Data.ByteString.Char8 ({- IsString -})
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Prelude
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types    as HTTP

data HeadObject
    = HeadObject {
        hoBucket :: Bucket
      , hoObjectName :: Object
      , hoVersionId :: Maybe T.Text
      , hoIfMatch :: Maybe T.Text
      -- ^ Return the object only if its entity tag (ETag, which is an md5sum of the content) is the same as the one specified; otherwise, catch a 'StatusCodeException' with a status of 412 precondition failed.
      , hoIfNoneMatch :: Maybe T.Text
      -- ^ Return the object only if its entity tag (ETag, which is an md5sum of the content) is different from the one specified; otherwise, catch a 'StatusCodeException' with a status of 304 not modified.
      }
  deriving (Show)

headObject :: Bucket -> T.Text -> HeadObject
headObject b o = HeadObject b o Nothing Nothing Nothing

data HeadObjectResponse
    = HeadObjectResponse {
        horMetadata :: Maybe ObjectMetadata
      }

data HeadObjectMemoryResponse
    = HeadObjectMemoryResponse (Maybe ObjectMetadata)
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery HeadObject where
    type ServiceConfiguration HeadObject = S3Configuration
    signQuery HeadObject {..} = s3SignQuery S3Query {
                                   s3QMethod = Head
                                 , s3QBucket = Just $ T.encodeUtf8 hoBucket
                                 , s3QObject = Just $ T.encodeUtf8 hoObjectName
                                 , s3QSubresources = HTTP.toQuery [
                                                       ("versionId" :: B8.ByteString,) <$> hoVersionId
                                                     ]
                                 , s3QQuery = []
                                 , s3QContentType = Nothing
                                 , s3QContentMd5 = Nothing
                                 , s3QAmzHeaders = []
                                 , s3QOtherHeaders = catMaybes [
                                                       ("if-match",) . T.encodeUtf8 <$> hoIfMatch
                                                     , ("if-none-match",) . T.encodeUtf8 <$> hoIfNoneMatch
                                                     ]
                                 , s3QRequestBody = Nothing
                                 }

instance ResponseConsumer HeadObject HeadObjectResponse where
    type ResponseMetadata HeadObjectResponse = S3Metadata
    responseConsumer httpReq HeadObject{..} _ resp
        | status == HTTP.status200 = HeadObjectResponse . Just <$> parseObjectMetadata headers
        | status == HTTP.status404 = return $ HeadObjectResponse Nothing
        | otherwise = throwStatusCodeException httpReq resp
      where
        status  = HTTP.responseStatus    resp
        headers = HTTP.responseHeaders   resp

instance Transaction HeadObject HeadObjectResponse

instance AsMemoryResponse HeadObjectResponse where
    type MemoryResponse HeadObjectResponse = HeadObjectMemoryResponse
    loadToMemory (HeadObjectResponse om) = return (HeadObjectMemoryResponse om)
