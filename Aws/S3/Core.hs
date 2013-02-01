module Aws.S3.Core where

import           Aws.Core
import           Control.Arrow                  ((***))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attempt                   (Attempt(..))
import           Data.Conduit                   (($$+-))
import           Data.Function
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.Typeable
import           System.Locale
import           Text.XML.Cursor                (($/), (&|))
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Control.Exception              as C
import qualified Control.Failure                as F
import qualified Crypto.Hash.MD5                as MD5
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Base64         as Base64
import qualified Data.CaseInsensitive           as CI
import qualified Data.Conduit                   as C
import qualified Data.Serialize                 as Serialize
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

data S3Configuration qt
    = S3Configuration {
        s3Protocol :: Protocol
      , s3Endpoint :: B.ByteString
      , s3RequestStyle :: RequestStyle
      , s3Port :: Int
      , s3UseUri :: Bool
      , s3DefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

instance DefaultServiceConfiguration (S3Configuration NormalQuery) where
  defServiceConfig = s3 HTTPS s3EndpointUsClassic False
  
  debugServiceConfig = s3 HTTP s3EndpointUsClassic False

instance DefaultServiceConfiguration (S3Configuration UriOnlyQuery) where
  defServiceConfig = s3 HTTPS s3EndpointUsClassic True
  debugServiceConfig = s3 HTTP s3EndpointUsClassic True

s3EndpointUsClassic :: B.ByteString
s3EndpointUsClassic = "s3.amazonaws.com"

s3EndpointUsWest :: B.ByteString
s3EndpointUsWest = "s3-us-west-1.amazonaws.com"

s3EndpointUsWest2 :: B.ByteString
s3EndpointUsWest2 = "s3-us-west-2.amazonaws.com"

s3EndpointEu :: B.ByteString
s3EndpointEu = "s3-eu-west-1.amazonaws.com"

s3EndpointApSouthEast :: B.ByteString
s3EndpointApSouthEast = "s3-ap-southeast-1.amazonaws.com"

s3EndpointApNorthEast :: B.ByteString
s3EndpointApNorthEast = "s3-ap-northeast-1.amazonaws.com"

s3 :: Protocol -> B.ByteString -> Bool -> S3Configuration qt
s3 protocol endpoint uri 
    = S3Configuration { 
         s3Protocol = protocol
       , s3Endpoint = endpoint
       , s3RequestStyle = BucketStyle
       , s3Port = defaultPort protocol
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
      , s3QContentMd5 :: Maybe MD5.MD5
      , s3QAmzHeaders :: HTTP.RequestHeaders
      , s3QOtherHeaders :: HTTP.RequestHeaders
      , s3QRequestBody :: Maybe (HTTP.RequestBody (C.ResourceT IO))
      }

instance Show S3Query where
    show S3Query{..} = "S3Query [" ++
                       " method: " ++ show s3QMethod ++
                       " ; bucket: " ++ show s3QBucket ++
                       " ; subresources: " ++ show s3QSubresources ++
                       " ; query: " ++ show s3QQuery ++
                       " ; request body: " ++ (case s3QRequestBody of Nothing -> "no"; _ -> "yes") ++
                       "]"

s3SignQuery :: S3Query -> S3Configuration qt -> SignatureData -> SignedQuery
s3SignQuery S3Query{..} S3Configuration{..} SignatureData{..}
    = SignedQuery {
        sqMethod = s3QMethod
      , sqProtocol = s3Protocol
      , sqHost = B.intercalate "." $ catMaybes host
      , sqPort = s3Port
      , sqPath = mconcat $ catMaybes path
      , sqQuery = sortedSubresources ++ s3QQuery ++ authQuery :: HTTP.Query
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
      amzHeaders = merge $ sortBy (compare `on` fst) s3QAmzHeaders
          where merge (x1@(k1,v1):x2@(k2,v2):xs) | k1 == k2  = merge ((k1, B8.intercalate "," [v1, v2]) : xs)
                                                 | otherwise = x1 : merge (x2 : xs)
                merge xs = xs

      (host, path) = case s3RequestStyle of
                       PathStyle   -> ([Just s3Endpoint], [Just "/", fmap (`B8.snoc` '/') s3QBucket, s3QObject])
                       BucketStyle -> ([s3QBucket, Just s3Endpoint], [Just "/", s3QObject])
                       VHostStyle  -> ([Just $ fromMaybe s3Endpoint s3QBucket], [Just "/", s3QObject])
      sortedSubresources = sort s3QSubresources
      canonicalizedResource = Blaze8.fromChar '/' `mappend`
                              maybe mempty (\s -> Blaze.copyByteString s `mappend` Blaze8.fromChar '/') s3QBucket `mappend`
                              maybe mempty Blaze.copyByteString s3QObject `mappend`
                              HTTP.renderQueryBuilder True sortedSubresources
      ti = case (s3UseUri, signatureTimeInfo) of
             (False, ti') -> ti'
             (True, AbsoluteTimestamp time) -> AbsoluteExpires $ s3DefaultExpiry `addUTCTime` time
             (True, AbsoluteExpires time) -> AbsoluteExpires time
      sig = signature signatureCredentials HmacSHA1 stringToSign
      stringToSign = Blaze.toByteString . mconcat . intersperse (Blaze8.fromChar '\n') . concat  $
                       [[Blaze.copyByteString $ httpMethod s3QMethod]
                       , [maybe mempty (Blaze.copyByteString . Base64.encode . Serialize.encode) s3QContentMd5]
                       , [maybe mempty Blaze.copyByteString s3QContentType]
                       , [Blaze.copyByteString $ case ti of
                                                   AbsoluteTimestamp time -> fmtRfc822Time time
                                                   AbsoluteExpires time -> fmtTimeEpochSeconds time]
                       , map amzHeader amzHeaders
                       , [canonicalizedResource]
                       ]
          where amzHeader (k, v) = Blaze.copyByteString (CI.foldedCase k) `mappend` Blaze8.fromChar ':' `mappend` Blaze.copyByteString v
      (authorization, authQuery) = case ti of
                                 AbsoluteTimestamp _ -> (Just $ B.concat ["AWS ", accessKeyID signatureCredentials, ":", sig], [])
                                 AbsoluteExpires time -> (Nothing, HTTP.toQuery $ makeAuthQuery time)
      makeAuthQuery time
          = [("Expires" :: B8.ByteString, fmtTimeEpochSeconds time)
            , ("AWSAccessKeyId", accessKeyID signatureCredentials)
            , ("SignatureMethod", "HmacSHA256")
            , ("Signature", sig)]

s3ResponseConsumer :: HTTPResponseConsumer a
                   -> IORef S3Metadata
                   -> HTTPResponseConsumer a
s3ResponseConsumer inner metadata resp = do
      let headerString = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders resp)
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"

      let m = S3Metadata { s3MAmzId2 = amzId2, s3MRequestId = requestId }
      liftIO $ tellMetadataRef metadata m

      if HTTP.responseStatus resp >= HTTP.status400
        then s3ErrorResponseConsumer resp
        else inner resp

s3XmlResponseConsumer :: (Cu.Cursor -> Response S3Metadata a)
                      -> IORef S3Metadata
                      -> HTTPResponseConsumer a
s3XmlResponseConsumer parse metadataRef =
    s3ResponseConsumer (xmlCursorConsumer parse metadataRef) metadataRef

s3BinaryResponseConsumer :: HTTPResponseConsumer a
                         -> IORef S3Metadata
                         -> HTTPResponseConsumer a
s3BinaryResponseConsumer inner metadataRef = s3ResponseConsumer inner metadataRef

s3ErrorResponseConsumer :: HTTPResponseConsumer a
s3ErrorResponseConsumer resp
    = do doc <- HTTP.responseBody resp $$+- XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         liftIO $ case parseError cursor of
           Success err      -> C.monadThrow err
           Failure otherErr -> C.monadThrow otherErr
    where
      parseError :: Cu.Cursor -> Attempt S3Error
      parseError root = do code <- force "Missing error Code" $ root $/ elContent "Code"
                           message <- force "Missing error Message" $ root $/ elContent "Message"
                           let resource = listToMaybe $ root $/ elContent "Resource"
                               hostId = listToMaybe $ root $/ elContent "HostId"
                               accessKeyId = listToMaybe $ root $/ elContent "AWSAccessKeyId"
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
                                      }

type CanonicalUserId = T.Text

data UserInfo
    = UserInfo {
        userId          :: CanonicalUserId
      , userDisplayName :: T.Text
      }
    deriving (Show)

parseUserInfo :: F.Failure XmlException m => Cu.Cursor -> m UserInfo
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
    | ReducedRedundancy
    deriving (Show)

parseStorageClass :: F.Failure XmlException m => T.Text -> m StorageClass
parseStorageClass "STANDARD"           = return Standard
parseStorageClass "REDUCED_REDUNDANCY" = return ReducedRedundancy
parseStorageClass s = F.failure . XmlException $ "Invalid Storage Class: " ++ T.unpack s

writeStorageClass :: StorageClass -> T.Text
writeStorageClass Standard          = "STANDARD"
writeStorageClass ReducedRedundancy = "REDUCED_REDUNDANCY"

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
      , objectOwner        :: UserInfo
      }
    deriving (Show)

parseObjectInfo :: F.Failure XmlException m => Cu.Cursor -> m ObjectInfo
parseObjectInfo el
    = do key <- force "Missing object Key" $ el $/ elContent "Key"
         let time s = case parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ T.unpack s of
                        Nothing -> F.failure $ XmlException "Invalid time"
                        Just v -> return v
         lastModified <- forceM "Missing object LastModified" $ el $/ elContent "LastModified" &| time
         eTag <- force "Missing object ETag" $ el $/ elContent "ETag"
         size <- forceM "Missing object Size" $ el $/ elContent "Size" &| textReadInt
         storageClass <- forceM "Missing object StorageClass" $ el $/ elContent "StorageClass" &| parseStorageClass
         owner <- forceM "Missing object Owner" $ el $/ Cu.laxElement "Owner" &| parseUserInfo
         return ObjectInfo{
                      objectKey          = key
                    , objectLastModified = lastModified
                    , objectETag         = eTag
                    , objectSize         = size
                    , objectStorageClass = storageClass
                    , objectOwner        = owner
                    }

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
      , omServerSideEncryption :: Maybe T.Text
      }
    deriving (Show)

parseObjectMetadata :: F.Failure HeaderException m => HTTP.ResponseHeaders -> m ObjectMetadata
parseObjectMetadata h = ObjectMetadata
                        `liftM` deleteMarker
                        `ap` etag
                        `ap` lastModified
                        `ap` return versionId
--                        `ap` expiration
                        `ap` return userMetadata
                        `ap` return missingUserMetadata
                        `ap` return serverSideEncryption
  where deleteMarker = case B8.unpack `fmap` lookup "x-amz-delete-marker" h of
                         Nothing -> return False
                         Just "true" -> return True
                         Just "false" -> return False
                         Just x -> F.failure $ HeaderException ("Invalid x-amz-delete-marker " ++ x)
        etag = case T.decodeUtf8 `fmap` lookup "ETag" h of
                 Just x -> return x
                 Nothing -> F.failure $ HeaderException "ETag missing"
        lastModified = case B8.unpack `fmap` lookup "Last-Modified" h of
                         Just ts -> case parseHttpDate ts of
                                      Just t -> return t
                                      Nothing -> F.failure $ HeaderException ("Invalid Last-Modified: " ++ ts)
                         Nothing -> F.failure $ HeaderException "Last-Modified missing"
        versionId = T.decodeUtf8 `fmap` lookup "x-amz-version-id" h
        -- expiration = return undefined
        userMetadata = flip mapMaybe ht $
                       \(k, v) -> do i <- T.stripPrefix "x-amz-meta-" k
                                     return (i, v)
        missingUserMetadata = T.decodeUtf8 `fmap` lookup "x-amz-missing-meta" h
        serverSideEncryption = T.decodeUtf8 `fmap` lookup "x-amz-server-side-encryption" h

        ht = map ((T.decodeUtf8 . CI.foldedCase) *** T.decodeUtf8) h

type LocationConstraint = T.Text

locationUsClassic, locationUsWest, locationUsWest2, locationEu, locationApSouthEast, locationApNorthEast :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationUsWest2 = "us-west-2"
locationEu = "EU"
locationApSouthEast = "ap-southeast-1"
locationApNorthEast = "ap-northeast-1"
