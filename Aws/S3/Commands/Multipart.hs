module Aws.S3.Commands.Multipart
       where
import           Aws.Aws
import           Aws.Core
import           Aws.S3.Core
import           Control.Applicative
import           Control.Arrow         (second)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Crypto.Hash
import           Data.ByteString.Char8 ({- IsString -})
import           Data.Conduit
import qualified Data.Conduit.List     as CL
import           Data.Maybe
import           Text.XML.Cursor       (($/))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import qualified Data.CaseInsensitive  as CI
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.HTTP.Types    as HTTP
import qualified Text.XML              as XML

{-
Aws supports following 6 api for Multipart-Upload.
Currently this code does not support number 3 and 6.

1. Initiate Multipart Upload
2. Upload Part
3. Upload Part - Copy
4. Complete Multipart Upload
5. Abort Multipart Upload
6. List Parts

-}

data InitiateMultipartUpload
  = InitiateMultipartUpload {
      imuBucket :: Bucket
    , imuObjectName :: Object
    , imuCacheControl :: Maybe T.Text
    , imuContentDisposition :: Maybe T.Text
    , imuContentEncoding :: Maybe T.Text
    , imuContentType :: Maybe T.Text
    , imuExpires :: Maybe Int
    , imuMetadata :: [(T.Text,T.Text)]
    , imuStorageClass :: Maybe StorageClass
    , imuWebsiteRedirectLocation :: Maybe T.Text
    , imuAcl :: Maybe CannedAcl
    , imuServerSideEncryption :: Maybe ServerSideEncryption
    , imuAutoMakeBucket :: Bool -- ^ Internet Archive S3 nonstandard extension
    }
  deriving (Show)

postInitiateMultipartUpload :: Bucket -> T.Text -> InitiateMultipartUpload
postInitiateMultipartUpload b o =
  InitiateMultipartUpload
    b o
    Nothing Nothing Nothing Nothing Nothing
    [] Nothing Nothing Nothing Nothing
    False

data InitiateMultipartUploadResponse
  = InitiateMultipartUploadResponse {
      imurBucket   :: !Bucket
    , imurKey      :: !T.Text
    , imurUploadId :: !T.Text
    }

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery InitiateMultipartUpload where
    type ServiceConfiguration InitiateMultipartUpload = S3Configuration
    signQuery InitiateMultipartUpload {..} = s3SignQuery S3Query {
        s3QMethod = Post
      , s3QBucket = Just $ T.encodeUtf8 imuBucket
      , s3QObject = Just $ T.encodeUtf8 $ imuObjectName
      , s3QSubresources = HTTP.toQuery[ ("uploads" :: B8.ByteString , Nothing :: Maybe B8.ByteString)]
      , s3QQuery = []
      , s3QContentType = T.encodeUtf8 <$> imuContentType
      , s3QContentMd5 = Nothing
      , s3QAmzHeaders = map (second T.encodeUtf8) $ catMaybes [
          ("x-amz-acl",) <$> writeCannedAcl <$> imuAcl
        , ("x-amz-storage-class",) <$> writeStorageClass <$> imuStorageClass
        , ("x-amz-website-redirect-location",) <$> imuWebsiteRedirectLocation
        , ("x-amz-server-side-encryption",) <$> writeServerSideEncryption <$> imuServerSideEncryption
        , if imuAutoMakeBucket then Just ("x-amz-auto-make-bucket", "1")  else Nothing
        ] ++ map( \x -> (CI.mk . T.encodeUtf8 $ T.concat ["x-amz-meta-", fst x], snd x)) imuMetadata
      , s3QOtherHeaders = map (second T.encodeUtf8) $ catMaybes [
          ("Expires",) . T.pack . show <$> imuExpires
        , ("Cache-Control",) <$> imuCacheControl
        , ("Content-Disposition",) <$> imuContentDisposition
        , ("Content-Encoding",) <$> imuContentEncoding
        ]
      , s3QRequestBody = Nothing
      }

instance ResponseConsumer r InitiateMultipartUploadResponse where
    type ResponseMetadata InitiateMultipartUploadResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse cursor
                  = do bucket <- force "Missing Bucket Name" $ cursor $/ elContent "Bucket"
                       key <- force "Missing Key" $ cursor $/ elContent "Key"
                       uploadId <- force "Missing UploadID" $ cursor $/ elContent "UploadId"
                       return InitiateMultipartUploadResponse{
                                                imurBucket         = bucket
                                              , imurKey            = key
                                              , imurUploadId       = uploadId
                                              }

instance Transaction InitiateMultipartUpload InitiateMultipartUploadResponse

instance AsMemoryResponse InitiateMultipartUploadResponse where
    type MemoryResponse InitiateMultipartUploadResponse = InitiateMultipartUploadResponse
    loadToMemory = return


----------------------------------



data UploadPart = UploadPart {
    upObjectName :: T.Text
  , upBucket :: Bucket
  , upPartNumber :: Integer
  , upUploadId :: T.Text
  , upContentType :: Maybe B8.ByteString
  , upContentMD5 :: Maybe (Digest MD5)
  , upServerSideEncryption :: Maybe ServerSideEncryption
  , upRequestBody  :: HTTP.RequestBody
  , upExpect100Continue :: Bool -- ^ Note: Requires http-client >= 0.4.10
}

uploadPart :: Bucket -> T.Text -> Integer -> T.Text -> HTTP.RequestBody -> UploadPart
uploadPart bucket obj p i body =
  UploadPart obj bucket p i
  Nothing Nothing Nothing body False

data UploadPartResponse
  = UploadPartResponse {
      uprVersionId :: !(Maybe T.Text),
      uprETag :: !T.Text
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
                                   ("x-amz-server-side-encryption",) <$> writeServerSideEncryption <$> upServerSideEncryption
                                 ]
                               , s3QOtherHeaders = catMaybes [
                                    if upExpect100Continue
                                        then Just ("Expect", "100-continue")
                                        else Nothing
                                 ]
                               , s3QRequestBody = Just upRequestBody
                               }

instance ResponseConsumer UploadPart UploadPartResponse where
    type ResponseMetadata UploadPartResponse = S3Metadata
    responseConsumer _ = s3ResponseConsumer $ \resp -> do
      let vid = T.decodeUtf8 `fmap` lookup "x-amz-version-id" (HTTP.responseHeaders resp)
      let etag = fromMaybe "" $ T.decodeUtf8 `fmap` lookup "ETag" (HTTP.responseHeaders resp)
      return $ UploadPartResponse vid etag

instance Transaction UploadPart UploadPartResponse

instance AsMemoryResponse UploadPartResponse where
    type MemoryResponse UploadPartResponse = UploadPartResponse
    loadToMemory = return

----------------------------



data CompleteMultipartUpload
  = CompleteMultipartUpload {
      cmuBucket :: Bucket
    , cmuObjectName :: Object
    , cmuUploadId :: T.Text
    , cmuPartNumberAndEtags :: [(Integer,T.Text)]
    , cmuExpiration :: Maybe T.Text
    , cmuServerSideEncryption :: Maybe T.Text
    , cmuServerSideEncryptionCustomerAlgorithm :: Maybe T.Text
    , cmuVersionId :: Maybe T.Text
    }
  deriving (Show)

postCompleteMultipartUpload :: Bucket -> T.Text -> T.Text -> [(Integer,T.Text)]-> CompleteMultipartUpload
postCompleteMultipartUpload b o i p = CompleteMultipartUpload b o i p Nothing  Nothing  Nothing  Nothing

data CompleteMultipartUploadResponse
  = CompleteMultipartUploadResponse {
      cmurLocation :: !T.Text
    , cmurBucket   :: !Bucket
    , cmurKey      :: !T.Text
    , cmurETag     :: !T.Text
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
      , s3QAmzHeaders = catMaybes [ ("x-amz-expiration",) <$> (T.encodeUtf8 <$> cmuExpiration)
                                  , ("x-amz-server-side-encryption",) <$> (T.encodeUtf8 <$> cmuServerSideEncryption)
                                  , ("x-amz-server-side-encryption-customer-algorithm",)
                                    <$> (T.encodeUtf8 <$> cmuServerSideEncryptionCustomerAlgorithm)
                                  , ("x-amz-version-id",) <$> (T.encodeUtf8 <$> cmuVersionId)
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

----------------------------



data AbortMultipartUpload
  = AbortMultipartUpload {
      amuBucket :: Bucket
    , amuObjectName :: Object
    , amuUploadId :: T.Text
    }
  deriving (Show)

postAbortMultipartUpload :: Bucket -> T.Text -> T.Text -> AbortMultipartUpload
postAbortMultipartUpload b o i = AbortMultipartUpload b o i

data AbortMultipartUploadResponse
  = AbortMultipartUploadResponse {
    }

-- | ServiceConfiguration: 'S3Configuration'
instance SignQuery AbortMultipartUpload where
    type ServiceConfiguration AbortMultipartUpload = S3Configuration
    signQuery AbortMultipartUpload {..} = s3SignQuery S3Query {
      s3QMethod = Delete
      , s3QBucket = Just $ T.encodeUtf8 amuBucket
      , s3QObject = Just $ T.encodeUtf8 amuObjectName
      , s3QSubresources = HTTP.toQuery[
        ("uploadId" :: B8.ByteString, Just amuUploadId :: Maybe T.Text)
        ]
      , s3QQuery = []
      , s3QContentType = Nothing
      , s3QContentMd5 = Nothing
      , s3QAmzHeaders = []
      , s3QOtherHeaders = []
      , s3QRequestBody = Nothing
      }

instance ResponseConsumer r AbortMultipartUploadResponse where
    type ResponseMetadata AbortMultipartUploadResponse = S3Metadata

    responseConsumer _ = s3XmlResponseConsumer parse
        where parse _cursor
                  = return AbortMultipartUploadResponse {}

instance Transaction AbortMultipartUpload AbortMultipartUploadResponse


instance AsMemoryResponse AbortMultipartUploadResponse where
    type MemoryResponse AbortMultipartUploadResponse = AbortMultipartUploadResponse
    loadToMemory = return


----------------------------

getUploadId ::
  Configuration
  -> S3Configuration NormalQuery
  -> HTTP.Manager
  -> T.Text
  -> T.Text
  -> IO T.Text
getUploadId cfg s3cfg mgr bucket object = do
  InitiateMultipartUploadResponse {
      imurBucket = _bucket
    , imurKey = _object'
    , imurUploadId = uploadId
    } <- memoryAws cfg s3cfg mgr $ postInitiateMultipartUpload bucket object
  return uploadId


sendEtag  ::
  Configuration
  -> S3Configuration NormalQuery
  -> HTTP.Manager
  -> T.Text
  -> T.Text
  -> T.Text
  -> [T.Text]
  -> IO ()
sendEtag cfg s3cfg mgr bucket object uploadId etags = do
  _ <- memoryAws cfg s3cfg mgr $
       postCompleteMultipartUpload bucket object uploadId (zip [1..] etags)
  return ()

putConduit ::
  MonadResource m =>
  Configuration
  -> S3Configuration NormalQuery
  -> HTTP.Manager
  -> T.Text
  -> T.Text
  -> T.Text
  -> Conduit BL.ByteString m T.Text
putConduit cfg s3cfg mgr bucket object uploadId = loop 1
  where
    loop n = do
      v' <- await
      case v' of
        Just v -> do
          UploadPartResponse _ etag <- memoryAws cfg s3cfg mgr $
            uploadPart bucket object n uploadId (HTTP.RequestBodyLBS v)
          yield etag
          loop (n+1)
        Nothing -> return ()

chunkedConduit :: (MonadResource m) => Integer -> Conduit B8.ByteString m BL.ByteString
chunkedConduit size = loop 0 []
  where
    loop :: Monad m => Integer -> [B8.ByteString] -> Conduit B8.ByteString m BL.ByteString
    loop cnt str = await >>= maybe (yieldChunk str) go
      where
        go :: Monad m => B8.ByteString -> Conduit B8.ByteString m BL.ByteString
        go line
          | size <= len = yieldChunk newStr >> loop 0 []
          | otherwise   = loop len newStr
          where
            len = fromIntegral (B8.length line) + cnt
            newStr = line:str

    yieldChunk :: Monad m => [B8.ByteString] -> Conduit i m BL.ByteString
    yieldChunk = yield . BL.fromChunks . reverse

multipartUpload ::
  Configuration
  -> S3Configuration NormalQuery
  -> HTTP.Manager
  -> T.Text
  -> T.Text
  -> Conduit () (ResourceT IO) B8.ByteString
  -> Integer
  -> ResourceT IO ()
multipartUpload cfg s3cfg mgr bucket object src chunkSize = do
  uploadId <- liftIO $ getUploadId cfg s3cfg mgr bucket object
  etags <- src
           $= chunkedConduit chunkSize
           $= putConduit cfg s3cfg mgr bucket object uploadId
           $$ CL.consume
  liftIO $ sendEtag cfg s3cfg mgr bucket object uploadId etags

multipartUploadSink :: MonadResource m
  => Configuration
  -> S3Configuration NormalQuery
  -> HTTP.Manager
  -> T.Text    -- ^ Bucket name
  -> T.Text    -- ^ Object name
  -> Integer   -- ^ chunkSize (minimum: 5MB)
  -> Sink B8.ByteString m ()
multipartUploadSink cfg s3cfg = multipartUploadSinkWithInitiator cfg s3cfg postInitiateMultipartUpload

multipartUploadWithInitiator ::
  Configuration
  -> S3Configuration NormalQuery
  -> (Bucket -> T.Text -> InitiateMultipartUpload)
  -> HTTP.Manager
  -> T.Text
  -> T.Text
  -> Conduit () (ResourceT IO) B8.ByteString
  -> Integer
  -> ResourceT IO ()
multipartUploadWithInitiator cfg s3cfg initiator mgr bucket object src chunkSize = do
  uploadId <- liftIO $ imurUploadId <$> memoryAws cfg s3cfg mgr (initiator bucket object)
  etags <- src
           $= chunkedConduit chunkSize
           $= putConduit cfg s3cfg mgr bucket object uploadId
           $$ CL.consume
  liftIO $ sendEtag cfg s3cfg mgr bucket object uploadId etags

multipartUploadSinkWithInitiator :: MonadResource m
  => Configuration
  -> S3Configuration NormalQuery
  -> (Bucket -> T.Text -> InitiateMultipartUpload) -- ^ Initiator
  -> HTTP.Manager
  -> T.Text    -- ^ Bucket name
  -> T.Text    -- ^ Object name
  -> Integer   -- ^ chunkSize (minimum: 5MB)
  -> Sink B8.ByteString m ()
multipartUploadSinkWithInitiator cfg s3cfg initiator mgr bucket object chunkSize = do
  uploadId <- liftIO $ imurUploadId <$> memoryAws cfg s3cfg mgr (initiator bucket object)
  etags <- chunkedConduit chunkSize
           $= putConduit cfg s3cfg mgr bucket object uploadId
           $= CL.consume
  liftIO $ sendEtag cfg s3cfg mgr bucket object uploadId etags
