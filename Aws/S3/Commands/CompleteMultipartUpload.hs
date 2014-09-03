module Aws.S3.Commands.CompleteMultipartUpload
       where

import           Aws.Core
import           Aws.S3.Core
import qualified Data.Map             as M
import           Control.Applicative
import           Data.Maybe (catMaybes)
import           Data.ByteString.Char8 ({- IsString -})
import           Text.XML.Cursor       (($/))
import qualified Text.XML             as XML
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types    as HTTP


data CompleteMultipartUpload
  = CompleteMultipartUpload {
    cmuBucket :: Bucket
    , cmuObjectName :: Object
    , cmuUploadId :: T.Text
    , cmuPartNumberAndEtags :: [(Integer,T.Text)]
    , cmuXAmzExpiration :: Maybe T.Text
    , cmuXAmzServerSideEncryption :: Maybe T.Text
    , cmuXAmzServerSideEncryptionCustomerAlgorithm :: Maybe T.Text
    , cmuXAmzVersionId :: Maybe T.Text
    }
  deriving (Show)

postCompleteMultipartUpload :: Bucket -> T.Text -> T.Text -> [(Integer,T.Text)]-> CompleteMultipartUpload
postCompleteMultipartUpload b o i p = CompleteMultipartUpload b o i p Nothing  Nothing  Nothing  Nothing

data CompleteMultipartUploadResponse
  = CompleteMultipartUploadResponse {
      cmurLocation :: T.Text
    , cmurBucket   :: Bucket
    , cmurKey      :: T.Text
    , cmurETag     :: T.Text
    }

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery CompleteMultipartUpload where
    type ServiceConfiguration CompleteMultipartUpload = S3Configuration
    signQuery CompleteMultipartUpload {..} = s3SignQuery S3Query {
      s3QMethod = Post
      , s3QBucket = Just $ T.encodeUtf8 cmuBucket
      , s3QObject = Just $ T.encodeUtf8 cmuObjectName
      , s3QSubresources = HTTP.toQuery[
        ("uploadId" :: B8.ByteString, Just cmuUploadId :: Maybe T.Text)
        ]
      , s3QQuery = []
      , s3QContentType = Nothing
      , s3QContentMd5 = Nothing
      , s3QAmzHeaders = catMaybes [ ("x-amz-expiration",) <$> (T.encodeUtf8 <$> cmuXAmzExpiration)
                                  , ("x-amz-server-side-encryption",) <$> (T.encodeUtf8 <$> cmuXAmzServerSideEncryption)
                                  , ("x-amz-server-side-encryption-customer-algorithm",)
                                    <$> (T.encodeUtf8 <$> cmuXAmzServerSideEncryptionCustomerAlgorithm)
                                  , ("x-amz-version-id",) <$> (T.encodeUtf8 <$> cmuXAmzVersionId)
                                  ]
      , s3QOtherHeaders = []
      , s3QRequestBody  = Just $ HTTP.RequestBodyLBS reqBody
      }
        where reqBody = XML.renderLBS XML.def XML.Document {
                    XML.documentPrologue = XML.Prologue [] Nothing []
                  , XML.documentRoot = root
                  , XML.documentEpilogue = []
                  }
              root = XML.Element {
                    XML.elementName = "CompleteMultipartUpload"
                  , XML.elementAttributes = M.empty
                  , XML.elementNodes = (partNode <$> cmuPartNumberAndEtags)
                  }
              partNode (partNumber, etag) = XML.NodeElement XML.Element {
                    XML.elementName = "Part"
                  , XML.elementAttributes = M.empty
                  , XML.elementNodes = [keyNode (T.pack (show partNumber)),etagNode etag]
                  }
              etagNode = toNode "ETag"
              keyNode     = toNode "PartNumber"
              toNode name content = XML.NodeElement XML.Element {
                    XML.elementName = name
                  , XML.elementAttributes = M.empty
                  , XML.elementNodes = [XML.NodeContent content]
                  }

instance ResponseConsumer r CompleteMultipartUploadResponse where
    type ResponseMetadata CompleteMultipartUploadResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse cursor
                  = do location <- force "Missing Location" $ cursor $/ elContent "Location"
                       bucket <- force "Missing Bucket Name" $ cursor $/ elContent "Bucket"
                       key <- force "Missing Key" $ cursor $/ elContent "Key"
                       etag <- force "Missing ETag" $ cursor $/ elContent "ETag"
                       return CompleteMultipartUploadResponse{
                                                cmurLocation       = location
                                              , cmurBucket         = bucket
                                              , cmurKey            = key
                                              , cmurETag           = etag
                                              }

instance Transaction CompleteMultipartUpload CompleteMultipartUploadResponse

instance AsMemoryResponse CompleteMultipartUploadResponse where
    type MemoryResponse CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    loadToMemory = return
