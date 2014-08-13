module Aws.S3.Commands.CopyObject
where

import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad.Trans.Resource (throwM)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Network.HTTP.Conduit as HTTP
import           Text.XML.Cursor (($/), (&|))
import           System.Locale

data CopyMetadataDirective = CopyMetadata | ReplaceMetadata [(T.Text,T.Text)]
  deriving (Show)

data CopyObject = CopyObject { coObjectName :: T.Text
                             , coBucket :: Bucket
                             , coSource :: ObjectId
                             , coMetadataDirective :: CopyMetadataDirective
                             , coIfMatch :: Maybe T.Text
                             , coIfNoneMatch :: Maybe T.Text
                             , coIfUnmodifiedSince :: Maybe UTCTime
                             , coIfModifiedSince :: Maybe UTCTime
                             , coStorageClass :: Maybe StorageClass
                             , coAcl :: Maybe CannedAcl
                             , coContentType :: Maybe B.ByteString
                             }
  deriving (Show)

copyObject :: Bucket -> T.Text -> ObjectId -> CopyMetadataDirective -> CopyObject
copyObject bucket obj src meta = CopyObject obj bucket src meta Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data CopyObjectResponse
  = CopyObjectResponse {
      corVersionId :: Maybe T.Text
    , corLastModified :: UTCTime
    , corETag :: T.Text
    }
  deriving (Show)

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery CopyObject where
    type ServiceConfiguration CopyObject = S3Configuration
    signQuery CopyObject {..} = s3SignQuery S3Query {
                                 s3QMethod = Put
                               , s3QBucket = Just $ T.encodeUtf8 coBucket
                               , s3QObject = Just $ T.encodeUtf8 coObjectName
                               , s3QSubresources = []
                               , s3QQuery = []
                               , s3QContentType = coContentType
                               , s3QContentMd5 = Nothing
                               , s3QAmzHeaders = map (second T.encodeUtf8) $ catMaybes [
                                   Just ("x-amz-copy-source",
                                         oidBucket `T.append` "/" `T.append` oidObject `T.append`
                                         case oidVersion of
                                           Nothing -> T.empty
                                           Just v -> "?versionId=" `T.append` v)
                                 , Just ("x-amz-metadata-directive", case coMetadataDirective of
                                            CopyMetadata -> "COPY"
                                            ReplaceMetadata _ -> "REPLACE")
                                 , ("x-amz-copy-source-if-match",)
                                   <$> coIfMatch
                                 , ("x-amz-copy-source-if-none-match",)
                                   <$> coIfNoneMatch
                                 , ("x-amz-copy-source-if-unmodified-since",)
                                   <$> textHttpDate <$> coIfUnmodifiedSince
                                 , ("x-amz-copy-source-if-modified-since",)
                                   <$> textHttpDate <$> coIfModifiedSince
                                 , ("x-amz-acl",)
                                   <$> writeCannedAcl <$> coAcl
                                 , ("x-amz-storage-class",)
                                   <$> writeStorageClass <$> coStorageClass
                                 ] ++ map ( \x -> (CI.mk . T.encodeUtf8 $
                                                   T.concat ["x-amz-meta-", fst x], snd x))
                                          coMetadata
                               , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes []
                               , s3QRequestBody = Nothing
                               }
      where coMetadata = case coMetadataDirective of
                           CopyMetadata -> []
                           ReplaceMetadata xs -> xs
            ObjectId{..} = coSource

instance ResponseConsumer CopyObject CopyObjectResponse where
    type ResponseMetadata CopyObjectResponse = S3Metadata
    responseConsumer _ mref = flip s3ResponseConsumer mref $ \resp -> do
        let vid = T.decodeUtf8 `fmap` lookup "x-amz-version-id" (HTTP.responseHeaders resp)
        (lastMod, etag) <- xmlCursorConsumer parse mref resp
        return $ CopyObjectResponse vid lastMod etag
      where parse el = do
              let parseHttpDate' x = case parseTime defaultTimeLocale iso8601UtcDate x of
                                       Nothing -> throwM $ XmlException ("Invalid Last-Modified " ++ x)
                                       Just y -> return y
              lastMod <- forceM "Missing Last-Modified" $ el $/ elContent "LastModified" &| (parseHttpDate' . T.unpack)
              etag <- force "Missing ETag" $ el $/ elContent "ETag"
              return (lastMod, etag)


instance Transaction CopyObject CopyObjectResponse

instance AsMemoryResponse CopyObjectResponse where
    type MemoryResponse CopyObjectResponse = CopyObjectResponse
    loadToMemory = return
