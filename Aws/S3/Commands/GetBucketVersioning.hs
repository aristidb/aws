module Aws.S3.Commands.GetBucketVersioning 
( 
  module Aws.S3.Commands.GetBucketVersioning
, VersioningState(..)
) where

import           Aws.Core
import           Aws.S3.Commands.PutBucketVersioning (VersioningState(..))
import           Aws.S3.Core
import           Control.Monad.Trans.Resource (throwM)
import           Network.HTTP.Types (toQuery)
import qualified Data.Text.Encoding   as T
import           Text.XML.Cursor (($.//))
import qualified Data.ByteString.Lazy.Char8 as B8

-- | Gets the versioning state of an existing bucket.
data GetBucketVersioning
    = GetBucketVersioning
      { gbvBucket :: Bucket
      }
    deriving (Show)

getBucketVersioning :: Bucket -> GetBucketVersioning
getBucketVersioning = GetBucketVersioning

data GetBucketVersioningResponse
    = GetBucketVersioningResponse
        { gbvVersioning :: Maybe VersioningState }
        -- ^ Nothing when the bucket is not versioned
    deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery GetBucketVersioning where
    type ServiceConfiguration GetBucketVersioning = S3Configuration

    signQuery GetBucketVersioning{..} = s3SignQuery $ S3Query
      { s3QMethod       = Get
      , s3QBucket       = Just $ T.encodeUtf8 gbvBucket
      , s3QSubresources = toQuery [("versioning" :: B8.ByteString, Nothing :: Maybe B8.ByteString)]
      , s3QQuery        = []
      , s3QContentType  = Nothing
      , s3QContentMd5   = Nothing
      , s3QObject       = Nothing
      , s3QAmzHeaders   = []
      , s3QOtherHeaders = []
      , s3QRequestBody  = Nothing
      }

instance ResponseConsumer r GetBucketVersioningResponse where
    type ResponseMetadata GetBucketVersioningResponse = S3Metadata

    responseConsumer _ _ = s3XmlResponseConsumer parse
      where parse cursor = do
              v <- case cursor $.// elContent "Status" of
                   [] -> return Nothing
                   ("Enabled":[]) -> return (Just VersioningEnabled)
                   ("Suspended":[]) -> return (Just VersioningSuspended)
                   _ -> throwM $ XmlException "Invalid Status"
              return GetBucketVersioningResponse { gbvVersioning = v }

instance Transaction GetBucketVersioning GetBucketVersioningResponse

instance AsMemoryResponse GetBucketVersioningResponse where
    type MemoryResponse GetBucketVersioningResponse = GetBucketVersioningResponse
    loadToMemory = return
