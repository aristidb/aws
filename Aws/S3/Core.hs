{-# LANGUAGE CPP, BangPatterns #-}
module Aws.S3.Core where

import           Aws.Core
import           Control.Arrow                  ((***))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource   (MonadThrow, throwM)
import           Crypto.Hash
import           Data.Byteable
import           Data.Conduit                   (($$+-))
import           Data.Function
import           Data.Functor                   ((<$>))
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Control.Applicative            ((<|>))
import           Data.Time
import           Data.Typeable
#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format
#else
import           System.Locale
#endif
import           Text.XML.Cursor                (($/), (&|))
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Control.Exception              as C
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Base64         as Base64
import qualified Data.CaseInsensitive           as CI
import qualified Data.Conduit                   as C
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP
import qualified Text.XML                       as XML
import qualified Text.XML.Cursor                as Cu

data S3Authorization
    = S3AuthorizationHeader
    | S3AuthorizationQuery
    deriving (Show)

data RequestStyle
    = PathStyle -- ^ Requires correctly setting region endpoint, but allows non-DNS compliant bucket names in the US standard region.
    | BucketStyle -- ^ Bucket name must be DNS compliant.
    | VHostStyle
    deriving (Show)

-- identical to DynamoDb.Core.Region
data Region = Region {
    rUri  :: B.ByteString
  , rName :: B.ByteString
  } deriving (Eq,Show,Read,Typeable)

data S3Configuration qt
    = S3Configuration {
        s3Protocol :: Protocol
      , s3Region :: Region
      -- ^ The regional endpoint. Ex: 's3UsEast1'
      , s3RequestStyle :: RequestStyle
      , s3Port :: Int
      , s3ServerSideEncryption :: Maybe ServerSideEncryption
      , s3UseUri :: Bool
      , s3DefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

instance DefaultServiceConfiguration (S3Configuration NormalQuery) where
  defServiceConfig = s3 HTTPS s3UsEast1 False
  debugServiceConfig = s3 HTTP s3UsEast1 False

instance DefaultServiceConfiguration (S3Configuration UriOnlyQuery) where
  defServiceConfig = s3 HTTPS s3UsEast1 True
  debugServiceConfig = s3 HTTP s3UsEast1 True

--------------------------------------------------------------------------------
-- | S3 Regions

s3UsEast1 :: Region -- ^ US East (N. Virginia)
s3UsEast1 = Region "s3-external-1.amazonaws.com" "us-east-1"

s3UsWest1 :: Region -- ^ US West (N. California)
s3UsWest1 = Region "s3-us-west-1.amazonaws.com" "us-west-1"

s3UsWest2 :: Region -- ^ US West (Oregon)
s3UsWest2 = Region "s3-us-west-2.amazonaws.com" "us-west-2"

s3EuWest1 :: Region -- ^ EU (Ireland)
s3EuWest1 = Region "s3-eu-west-1.amazonaws.com" "eu-west-1"

s3EuCentral1 :: Region -- ^ EU (Frankfurt)
s3EuCentral1 = Region "s3-eu-central-1.amazonaws.com" "eu-central-1"

s3ApNe1 :: Region -- ^ Asia Pacific (Tokyo)
s3ApNe1 = Region "s3-ap-northeast-1.amazonaws.com" "ap-northeast-1"

s3ApNe2 :: Region -- ^ Asia Pacific (Seoul)
s3ApNe2 = Region "s3-ap-northeast-2.amazonaws.com" "ap-northeast-2"

s3ApSe1 :: Region -- ^ Asia Pacific (Singapore)
s3ApSe1 = Region "s3-ap-southeast-1.amazonaws.com" "ap-southeast-1"

s3ApSe2 :: Region -- ^ Asia Pacific (Sydney)
s3ApSe2 = Region "s3-ap-southeast-2.amazonaws.com" "ap-southeast-2"

s3SaEast1 :: Region -- ^ South America (São Paulo)
s3SaEast1 = Region "s3-sa-east-1.amazonaws.com" "sa-east-1"

s3 :: Protocol -> Region -> Bool -> S3Configuration qt
s3 protocol region uri
    = S3Configuration {
         s3Protocol = protocol
       , s3Region = region
       , s3RequestStyle = BucketStyle
       , s3Port = defaultPort protocol
       , s3ServerSideEncryption = Nothing
       , s3UseUri = uri
       , s3DefaultExpiry = 15*60
       }

type ErrorCode = T.Text

data S3Error
    = S3Error {
        s3StatusCode :: HTTP.Status
      , s3ErrorCode :: ErrorCode -- Error/Code
      , s3ErrorMessage :: T.Text -- Error/Message
      , s3ErrorResource :: Maybe T.Text -- Error/Resource
      , s3ErrorHostId :: Maybe T.Text -- Error/HostId
      , s3ErrorAccessKeyId :: Maybe T.Text -- Error/AWSAccessKeyId
      , s3ErrorStringToSign :: Maybe B.ByteString -- Error/StringToSignBytes (hexadecimal encoding)
      , s3ErrorBucket :: Maybe T.Text -- Error/Bucket
      , s3ErrorEndpointRaw :: Maybe T.Text -- Error/Endpoint (i.e. correct bucket location)
      , s3ErrorEndpoint :: Maybe B.ByteString -- Error/Endpoint without the bucket prefix
      }
    deriving (Show, Typeable)

instance C.Exception S3Error

data S3Metadata
    = S3Metadata {
        s3MAmzId2 :: Maybe T.Text
      , s3MRequestId :: Maybe T.Text
      }
    deriving (Show, Typeable)

instance Monoid S3Metadata where
    mempty = S3Metadata Nothing Nothing
    S3Metadata a1 r1 `mappend` S3Metadata a2 r2 = S3Metadata (a1 `mplus` a2) (r1 `mplus` r2)

instance Loggable S3Metadata where
    toLogText (S3Metadata id2 rid) = "S3: request ID=" `mappend`
                                     fromMaybe "<none>" rid `mappend`
                                     ", x-amz-id-2=" `mappend`
                                     fromMaybe "<none>" id2

data S3Query
    = S3Query {
        s3QMethod :: Method
      , s3QBucket :: Maybe B.ByteString
      , s3QObject :: Maybe B.ByteString
      , s3QSubresources :: HTTP.Query
      , s3QQuery :: HTTP.Query
      , s3QContentType :: Maybe B.ByteString
      , s3QContentMd5 :: Maybe (Digest MD5)
      -- ^ The Content-MD5 header value.
      , s3QAmzHeaders :: HTTP.RequestHeaders
      , s3QOtherHeaders :: HTTP.RequestHeaders
#if MIN_VERSION_http_conduit(2, 0, 0)
      , s3QRequestBody :: Maybe HTTP.RequestBody
#else
      , s3QRequestBody :: Maybe (HTTP.RequestBody (C.ResourceT IO))
#endif
      }

instance Show S3Query where
    show S3Query{..} = "S3Query [" ++
                       " method: " ++ show s3QMethod ++
                       " ; bucket: " ++ show s3QBucket ++
                       " ; subresources: " ++ show s3QSubresources ++
                       " ; query: " ++ show s3QQuery ++
                       " ; request body: " ++ (case s3QRequestBody of Nothing -> "no"; _ -> "yes") ++
                       "]"

hAmzDate, hAmzContentSha256, hAmzAlgorithm, hAmzCredential, hAmzExpires, hAmzSignature, hAmzSecurityToken :: HTTP.HeaderName
hAmzDate = "X-Amz-Date"
hAmzContentSha256 = "X-Amz-content-Sha256"
hAmzAlgorithm = "X-Amz-Algorithm"
hAmzCredential = "X-Amz-Credential"
hAmzExpires = "X-Amz-Expires"
hAmzSignedHeaders = "X-Amz-SignedHeaders"
hAmzSignature = "X-Amz-Signature"
hAmzSecurityToken = "X-Amz-Security-Token"

-- | For signature v4 signing see
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html>
s3SignQuery :: S3Query -> S3Configuration qt -> SignatureData -> SignedQuery
s3SignQuery S3Query{..} S3Configuration{..} sd@SignatureData{..}
    = SignedQuery {
        sqMethod = s3QMethod
      , sqProtocol = s3Protocol
      , sqHost = B.intercalate "." $ catMaybes host
      , sqPort = s3Port
      , sqPath = mconcat $ catMaybes path
      , sqQuery = sortedSubresources ++ s3QQuery ++ (fmap (\(x, y) -> (CI.original x, Just y)) authQuery) :: HTTP.Query
      , sqDate = Just signatureTime
      , sqAuthorization = authorization
      , sqContentType = s3QContentType
      , sqContentMd5 = s3QContentMd5
      , sqAmzHeaders = amzHeaders
      , sqOtherHeaders = s3QOtherHeaders
      , sqBody = s3QRequestBody
      , sqStringToSign = stringToSign
      }
    where
      credentials = signatureCredentials

      -- hash of an empty string
      emptyBodyHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

      -- needs to match th eone produces in the @authorizationV4@
      sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime

      -- inject date and an empty content sha256, if not given.
      s3QAmzHeaders' = (hAmzDate, sigTime):case lookup hAmzContentSha256 s3QAmzHeaders of
        Just _ -> s3QAmzHeaders
        Nothing -> (hAmzContentSha256, emptyBodyHash):s3QAmzHeaders
      
      amzHeaders = merge $ sortBy (compare `on` fst) (s3QAmzHeaders' ++ iamTok)
          where merge (x1@(k1,v1):x2@(k2,v2):xs) | k1 == k2  = merge ((k1, B8.intercalate "," [v1, v2]) : xs)
                                                 | otherwise = x1 : merge (x2 : xs)
                merge xs = xs

      urlEncodedS3QObject = HTTP.urlEncode False <$> s3QObject
      (host, path) = case s3RequestStyle of
                       PathStyle   -> ([Just (rUri s3Region)], [Just "/", fmap (`B8.snoc` '/') s3QBucket, urlEncodedS3QObject])
                       BucketStyle -> ([s3QBucket, Just (rUri s3Region)], [Just "/", urlEncodedS3QObject])
                       VHostStyle  -> ([Just $ fromMaybe (rUri s3Region) s3QBucket], [Just "/", urlEncodedS3QObject])
      sortedSubresources = sort s3QSubresources

      ti = case (s3UseUri, signatureTimeInfo) of
             (False, ti') -> ti'
             (True, AbsoluteTimestamp time) -> AbsoluteExpires $ s3DefaultExpiry `addUTCTime` time
             (True, AbsoluteExpires time) -> AbsoluteExpires time

      iamTok = maybe [] (\x -> [(hAmzSecurityToken, x)]) (iamToken signatureCredentials)
      
      -- must provide host in the canonical headers.
      canonicalHeaders = sortBy (compare `on` fst) $ amzHeaders ++ catMaybes
                         [Just ("host", B.intercalate "." $ catMaybes host)
                         , ("content-type",) <$> s3QContentType
                         ]
      -- string to sign depends on the use case:
      -- The general structure is:
      --    <method>
      --    <path>
      --    <query string>
      --    <canonical headers - header1:value1\nheader2:value2...\n>
      --    <signed headers - header1;header2...>
      --    <hashed payload - sha256>
      --
      -- 1) Generate an Authentication Header <http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html>
      --    GET
      --    /text.txt
      --
      --    host:bucket.region.amazonaws.com
      --    x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
      --    x-amz-date:20130524T000000Z
      --
      --    host;x-amz-content-sha256;x-amz-date
      --    e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
      --
      -- 2) Generating a signed link with an expirydate <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html>
      --    GET
      --    /test.txt
      --    X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20130524%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20130524T000000Z&X-Amz-Expires=86400&X-Amz-SignedHeaders=host
      --    host:examplebucket.s3.amazonaws.com
      --
      --    host
      --    UNSIGNED-PAYLOAD
      --
      stringToSign = B.concat $ intercalate ["\n"] $
                       [ [httpMethod s3QMethod]               -- method
                       , [mconcat . catMaybes $ path]         -- path
                       , [HTTP.renderQuery False queryString] -- query string
                       ] ++
                       map (\(a,b) -> [CI.foldedCase a,":",b]) headers ++
                       [ [] -- end headers
                       , intersperse ";" (map (CI.foldedCase . fst) headers)
                       , [payloadHash]
                       ]

      (payloadHash, queryString, headers) = case ti of
        AbsoluteTimestamp _  -> (fromMaybe emptyBodyHash $ lookup hAmzContentSha256 amzHeaders, [], canonicalHeaders)
        AbsoluteExpires time -> ("UNSIGNED-PAYLOAD", HTTP.toQuery . fmap (\(x,y) -> (CI.original x, y)) $ makeAuthQuery time, [("host", B.intercalate "." $ catMaybes host)])

      auth = authorizationV4' sd HmacSHA256 (rName s3Region) "s3"
                       (B.concat (intersperse ";" (map (CI.foldedCase . fst) canonicalHeaders)))
                       stringToSign
      sig  = signatureV4 sd HmacSHA256 (rName s3Region) "s3" stringToSign

      (authorization, authQuery) = case ti of
        AbsoluteTimestamp _  -> (Just . return $ auth, [])
        AbsoluteExpires time -> (Nothing, (hAmzSignature, sig):makeAuthQuery time)

      makeAuthQuery time
          = [ (hAmzAlgorithm, "AWS4-HMAC-SHA256")
            , (hAmzCredential, credentialV4 sd (rName s3Region) "s3")
            , (hAmzDate, sigTime)
            , (hAmzExpires, B8.pack . show . floor $ diffUTCTime time signatureTime)
            , (hAmzSignedHeaders, "host") ] ++ iamTok

s3ResponseConsumer :: HTTPResponseConsumer a
                         -> IORef S3Metadata
                         -> HTTPResponseConsumer a
s3ResponseConsumer inner metadataRef = s3BinaryResponseConsumer inner' metadataRef
  where inner' resp =
          do
            !res <- inner resp
            C.closeResumableSource (HTTP.responseBody resp)
            return res

s3BinaryResponseConsumer :: HTTPResponseConsumer a
                   -> IORef S3Metadata
                   -> HTTPResponseConsumer a
s3BinaryResponseConsumer inner metadata resp = do
      let headerString = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders resp)
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"

      let m = S3Metadata { s3MAmzId2 = amzId2, s3MRequestId = requestId }
      liftIO $ tellMetadataRef metadata m

      if HTTP.responseStatus resp >= HTTP.status300
        then s3ErrorResponseConsumer resp
        else inner resp

s3XmlResponseConsumer :: (Cu.Cursor -> Response S3Metadata a)
                      -> IORef S3Metadata
                      -> HTTPResponseConsumer a
s3XmlResponseConsumer parse metadataRef =
    s3ResponseConsumer (xmlCursorConsumer parse metadataRef) metadataRef

s3ErrorResponseConsumer :: HTTPResponseConsumer a
s3ErrorResponseConsumer resp
    = do doc <- HTTP.responseBody resp $$+- XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         liftIO $ case parseError cursor of
           Right err      -> throwM err
           Left otherErr  -> throwM otherErr
    where
      parseError :: Cu.Cursor -> Either C.SomeException S3Error
      parseError root = do code <- force "Missing error Code" $ root $/ elContent "Code"
                           message <- force "Missing error Message" $ root $/ elContent "Message"
                           let resource = listToMaybe $ root $/ elContent "Resource"
                               hostId = listToMaybe $ root $/ elContent "HostId"
                               accessKeyId = listToMaybe $ root $/ elContent "AWSAccessKeyId"
                               bucket = listToMaybe $ root $/ elContent "Bucket"
                               endpointRaw = listToMaybe $ root $/ elContent "Endpoint"
                               endpoint = T.encodeUtf8 <$> (T.stripPrefix (fromMaybe "" bucket <> ".") =<< endpointRaw)
                               stringToSign = do unprocessed <- listToMaybe $ root $/ elCont "StringToSignBytes"
                                                 bytes <- mapM readHex2 $ words unprocessed
                                                 return $ B.pack bytes
                           return S3Error {
                                        s3StatusCode = HTTP.responseStatus resp
                                      , s3ErrorCode = code
                                      , s3ErrorMessage = message
                                      , s3ErrorResource = resource
                                      , s3ErrorHostId = hostId
                                      , s3ErrorAccessKeyId = accessKeyId
                                      , s3ErrorStringToSign = stringToSign
                                      , s3ErrorBucket = bucket
                                      , s3ErrorEndpointRaw = endpointRaw
                                      , s3ErrorEndpoint = endpoint
                                      }

type CanonicalUserId = T.Text

data UserInfo
    = UserInfo {
        userId          :: CanonicalUserId
      , userDisplayName :: T.Text
      }
    deriving (Show)

parseUserInfo :: MonadThrow m => Cu.Cursor -> m UserInfo
parseUserInfo el = do id_ <- force "Missing user ID" $ el $/ elContent "ID"
                      displayName <- force "Missing user DisplayName" $ el $/ elContent "DisplayName"
                      return UserInfo { userId = id_, userDisplayName = displayName }

data CannedAcl
    = AclPrivate
    | AclPublicRead
    | AclPublicReadWrite
    | AclAuthenticatedRead
    | AclBucketOwnerRead
    | AclBucketOwnerFullControl
    | AclLogDeliveryWrite
    deriving (Show)

writeCannedAcl :: CannedAcl -> T.Text
writeCannedAcl AclPrivate                = "private"
writeCannedAcl AclPublicRead             = "public-read"
writeCannedAcl AclPublicReadWrite        = "public-read-write"
writeCannedAcl AclAuthenticatedRead      = "authenticated-read"
writeCannedAcl AclBucketOwnerRead        = "bucket-owner-read"
writeCannedAcl AclBucketOwnerFullControl = "bucket-owner-full-control"
writeCannedAcl AclLogDeliveryWrite       = "log-delivery-write"

data StorageClass
    = Standard
    | StandardInfrequentAccess
    | ReducedRedundancy
    | Glacier
    | OtherStorageClass T.Text
    deriving (Show)

parseStorageClass :: T.Text -> StorageClass
parseStorageClass "STANDARD"           = Standard
parseStorageClass "STANDARD_IA"        = StandardInfrequentAccess
parseStorageClass "REDUCED_REDUNDANCY" = ReducedRedundancy
parseStorageClass "GLACIER"            = Glacier
parseStorageClass s                    = OtherStorageClass s

writeStorageClass :: StorageClass -> T.Text
writeStorageClass Standard                 = "STANDARD"
writeStorageClass StandardInfrequentAccess = "STANDARD_IA"
writeStorageClass ReducedRedundancy        = "REDUCED_REDUNDANCY"
writeStorageClass Glacier                  = "GLACIER"
writeStorageClass (OtherStorageClass s) = s

data ServerSideEncryption
    = AES256
    deriving (Show)

parseServerSideEncryption :: MonadThrow m => T.Text -> m ServerSideEncryption
parseServerSideEncryption "AES256" = return AES256
parseServerSideEncryption s = throwM . XmlException $ "Invalid Server Side Encryption: " ++ T.unpack s

writeServerSideEncryption :: ServerSideEncryption -> T.Text
writeServerSideEncryption AES256 = "AES256"

type Bucket = T.Text

data BucketInfo
    = BucketInfo {
        bucketName         :: Bucket
      , bucketCreationDate :: UTCTime
      }
    deriving (Show)

type Object = T.Text

data ObjectId
    = ObjectId {
        oidBucket :: Bucket
      , oidObject :: Object
      , oidVersion :: Maybe T.Text
      }
    deriving (Show)

data ObjectInfo
    = ObjectInfo {
        objectKey          :: T.Text
      , objectLastModified :: UTCTime
      , objectETag         :: T.Text
      , objectSize         :: Integer
      , objectStorageClass :: StorageClass
      , objectOwner        :: Maybe UserInfo
      }
    deriving (Show)

parseObjectInfo :: MonadThrow m => Cu.Cursor -> m ObjectInfo
parseObjectInfo el
    = do key <- force "Missing object Key" $ el $/ elContent "Key"
         let time s = case (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ T.unpack s) <|>
                           (parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" $ T.unpack s) of
                        Nothing -> throwM $ XmlException "Invalid time"
                        Just v -> return v
         lastModified <- forceM "Missing object LastModified" $ el $/ elContent "LastModified" &| time
         eTag <- force "Missing object ETag" $ el $/ elContent "ETag"
         size <- forceM "Missing object Size" $ el $/ elContent "Size" &| textReadInt
         storageClass <- forceM "Missing object StorageClass" $ el $/ elContent "StorageClass" &| return . parseStorageClass
         owner <- case el $/ Cu.laxElement "Owner" &| parseUserInfo of
                    (x:_) -> fmap' Just x
                    [] -> return Nothing
         return ObjectInfo{
                      objectKey          = key
                    , objectLastModified = lastModified
                    , objectETag         = eTag
                    , objectSize         = size
                    , objectStorageClass = storageClass
                    , objectOwner        = owner
                    }
    where
      fmap' :: Monad m => (a -> b) -> m a -> m b
      fmap' f ma = ma >>= return . f

data ObjectMetadata
    = ObjectMetadata {
        omDeleteMarker         :: Bool
      , omETag                 :: T.Text
      , omLastModified         :: UTCTime
      , omVersionId            :: Maybe T.Text
-- TODO:
--      , omExpiration           :: Maybe (UTCTime, T.Text)
      , omUserMetadata         :: [(T.Text, T.Text)]
      , omMissingUserMetadata  :: Maybe T.Text
      , omServerSideEncryption :: Maybe ServerSideEncryption
      }
    deriving (Show)

parseObjectMetadata :: MonadThrow m => HTTP.ResponseHeaders -> m ObjectMetadata
parseObjectMetadata h = ObjectMetadata
                        `liftM` deleteMarker
                        `ap` etag
                        `ap` lastModified
                        `ap` return versionId
--                        `ap` expiration
                        `ap` return userMetadata
                        `ap` return missingUserMetadata
                        `ap` serverSideEncryption
  where deleteMarker = case B8.unpack `fmap` lookup "x-amz-delete-marker" h of
                         Nothing -> return False
                         Just "true" -> return True
                         Just "false" -> return False
                         Just x -> throwM $ HeaderException ("Invalid x-amz-delete-marker " ++ x)
        etag = case T.decodeUtf8 `fmap` lookup "ETag" h of
                 Just x -> return x
                 Nothing -> throwM $ HeaderException "ETag missing"
        lastModified = case B8.unpack `fmap` lookup "Last-Modified" h of
                         Just ts -> case parseHttpDate ts of
                                      Just t -> return t
                                      Nothing -> throwM $ HeaderException ("Invalid Last-Modified: " ++ ts)
                         Nothing -> throwM $ HeaderException "Last-Modified missing"
        versionId = T.decodeUtf8 `fmap` lookup "x-amz-version-id" h
        -- expiration = return undefined
        userMetadata = flip mapMaybe ht $
                       \(k, v) -> do i <- T.stripPrefix "x-amz-meta-" k
                                     return (i, v)
        missingUserMetadata = T.decodeUtf8 `fmap` lookup "x-amz-missing-meta" h
        serverSideEncryption = case T.decodeUtf8 `fmap` lookup "x-amz-server-side-encryption" h of
                                 Just x -> return $ parseServerSideEncryption x
                                 Nothing -> return Nothing

        ht = map ((T.decodeUtf8 . CI.foldedCase) *** T.decodeUtf8) h

type LocationConstraint = T.Text

locationUsClassic, locationUsWest, locationUsWest2, locationEu, locationEuFrankfurt, locationApSouthEast, locationApSouthEast2, locationApNorthEast, locationSA :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationUsWest2 = "us-west-2"
locationEu = "EU"
locationEuFrankfurt = "eu-central-1"
locationApSouthEast = "ap-southeast-1"
locationApSouthEast2 = "ap-southeast-2"
locationApNorthEast = "ap-northeast-1"
locationSA = "sa-east-1"

normaliseLocation :: LocationConstraint -> LocationConstraint
normaliseLocation location
  | location == "eu-west-1" = locationEu
  | otherwise = location
