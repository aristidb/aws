{-# LANGUAGE CPP, BangPatterns #-}
module Aws.S3.Core where

import           Aws.Core
import           Control.Arrow                  (first, (***))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource   (MonadThrow, throwM)
import           Data.Char                      (isAscii, isAlphaNum, toUpper, ord)
import           Data.Conduit                   ((.|))
import           Data.Function
import           Data.Functor                   ((<$>))
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup                 as Sem
import           Control.Applicative            ((<|>))
import           Data.Time
import           Data.Typeable
import           Numeric                        (showHex)
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif
import           Text.XML.Cursor                (($/), (&|))
import qualified Data.Attoparsec.ByteString     as Atto
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Control.Exception              as C
import qualified Crypto.Hash                    as CH
import qualified Data.ByteArray                 as ByteArray
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Base16         as Base16
import qualified Data.ByteString.Base64         as Base64
import qualified Data.CaseInsensitive           as CI
import qualified Data.Conduit                   as C
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP
import qualified Text.XML                       as XML
import qualified Text.XML.Cursor                as Cu
import           Prelude

data S3Authorization
    = S3AuthorizationHeader
    | S3AuthorizationQuery
    deriving (Show)

data RequestStyle
    = PathStyle -- ^ Requires correctly setting region endpoint, but allows non-DNS compliant bucket names in the US standard region.
    | BucketStyle -- ^ Bucket name must be DNS compliant.
    | VHostStyle
    deriving (Show)

data S3SignPayloadMode
    = AlwaysUnsigned -- ^ Always use the "UNSIGNED-PAYLOAD" option.
    | SignWithEffort -- ^ Sign the payload when 'HTTP.RequestBody' is a on-memory one ('HTTP.RequestBodyLBS' or 'HTTP.RequestBodyBS'). Otherwise use the "UNSINGED-PAYLOAD" option.
    | AlwaysSigned   -- ^ Always sign the payload. Note: 'error' called when 'HTTP.RequestBody' is a streaming one.
    deriving (Eq, Show, Read, Typeable)

data S3SignVersion
    = S3SignV2
    | S3SignV4 { _s3SignPayloadMode :: S3SignPayloadMode }
    deriving (Eq, Show, Read, Typeable)

data S3Configuration qt
    = S3Configuration {
        s3Protocol :: Protocol
      , s3Endpoint :: B.ByteString
      , s3RequestStyle :: RequestStyle
      , s3Port :: Int
      , s3ServerSideEncryption :: Maybe ServerSideEncryption
      , s3UseUri :: Bool
      , s3DefaultExpiry :: NominalDiffTime
      , s3SignVersion :: S3SignVersion
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

s3EndpointEuWest2 :: B.ByteString
s3EndpointEuWest2 = "s3-eu-west-2.amazonaws.com"

s3EndpointApSouthEast :: B.ByteString
s3EndpointApSouthEast = "s3-ap-southeast-1.amazonaws.com"

s3EndpointApSouthEast2 :: B.ByteString
s3EndpointApSouthEast2 = "s3-ap-southeast-2.amazonaws.com"

s3EndpointApNorthEast :: B.ByteString
s3EndpointApNorthEast = "s3-ap-northeast-1.amazonaws.com"

s3 :: Protocol -> B.ByteString -> Bool -> S3Configuration qt
s3 protocol endpoint uri
    = S3Configuration {
         s3Protocol = protocol
       , s3Endpoint = endpoint
       , s3RequestStyle = BucketStyle
       , s3Port = defaultPort protocol
       , s3ServerSideEncryption = Nothing
       , s3UseUri = uri
       , s3DefaultExpiry = 15*60
       , s3SignVersion = S3SignV2
       }

s3v4 :: Protocol -> B.ByteString -> Bool -> S3SignPayloadMode -> S3Configuration qt
s3v4 protocol endpoint uri payload
    = S3Configuration
    { s3Protocol = protocol
    , s3Endpoint = endpoint
    , s3RequestStyle = BucketStyle
    , s3Port = defaultPort protocol
    , s3ServerSideEncryption = Nothing
    , s3UseUri = uri
    , s3DefaultExpiry = 15*60
    , s3SignVersion = S3SignV4 payload
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

instance Sem.Semigroup S3Metadata where
    S3Metadata a1 r1 <> S3Metadata a2 r2 = S3Metadata (a1 `mplus` a2) (r1 `mplus` r2)

instance Monoid S3Metadata where
    mempty = S3Metadata Nothing Nothing
    mappend = (Sem.<>)

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
      , s3QContentMd5 :: Maybe (CH.Digest CH.MD5)
      , s3QAmzHeaders :: HTTP.RequestHeaders
      , s3QOtherHeaders :: HTTP.RequestHeaders
      , s3QRequestBody :: Maybe HTTP.RequestBody
      }

instance Show S3Query where
    show S3Query{..} = "S3Query [" ++
                       " method: " ++ show s3QMethod ++
                       " ; bucket: " ++ show s3QBucket ++
                       " ; subresources: " ++ show s3QSubresources ++
                       " ; query: " ++ show s3QQuery ++
                       " ; request body: " ++ (case s3QRequestBody of Nothing -> "no"; _ -> "yes") ++
                       "]"

hAmzDate, hAmzContentSha256, hAmzAlgorithm, hAmzCredential, hAmzExpires, hAmzSignedHeaders, hAmzSignature, hAmzSecurityToken :: HTTP.HeaderName
hAmzDate          = "X-Amz-Date"
hAmzContentSha256 = "X-Amz-Content-Sha256"
hAmzAlgorithm     = "X-Amz-Algorithm"
hAmzCredential    = "X-Amz-Credential"
hAmzExpires       = "X-Amz-Expires"
hAmzSignedHeaders = "X-Amz-SignedHeaders"
hAmzSignature     = "X-Amz-Signature"
hAmzSecurityToken = "X-Amz-Security-Token"

s3SignQuery :: S3Query -> S3Configuration qt -> SignatureData -> SignedQuery
s3SignQuery S3Query{..} S3Configuration{ s3SignVersion = S3SignV2, .. } SignatureData{..}
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
      -- This also implements anonymous queries.
      isanon = isAnonymousCredentials signatureCredentials 
      amzHeaders = merge $ sortBy (compare `on` fst) $ s3QAmzHeaders ++ 
        if isanon 
          then []
          else fmap (\(k, v) -> (CI.mk k, v)) iamTok
          where merge (x1@(k1,v1):x2@(k2,v2):xs) | k1 == k2  = merge ((k1, B8.intercalate "," [v1, v2]) : xs)
                                                 | otherwise = x1 : merge (x2 : xs)
                merge xs = xs

      urlEncodedS3QObject = s3UriEncode False <$> s3QObject
      (host, path) = case s3RequestStyle of
                       PathStyle   -> ([Just s3Endpoint], [Just "/", fmap (`B8.snoc` '/') s3QBucket, urlEncodedS3QObject])
                       BucketStyle -> ([s3QBucket, Just s3Endpoint], [Just "/", urlEncodedS3QObject])
                       VHostStyle  -> ([Just $ fromMaybe s3Endpoint s3QBucket], [Just "/", urlEncodedS3QObject])
      sortedSubresources = sort s3QSubresources
      canonicalizedResource = Blaze8.fromChar '/' `mappend`
                              maybe mempty (\s -> Blaze.copyByteString s `mappend` Blaze8.fromChar '/') s3QBucket `mappend`
                              maybe mempty Blaze.copyByteString urlEncodedS3QObject `mappend`
                              encodeQuerySign sortedSubresources
      -- query parameters overriding response headers must not be URI encoded when calculating signature
      -- http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#ConstructingTheCanonicalizedResourceElement
      -- Note this is limited to amazon auth version 2 in the new auth version 4 this weird exception is not present
      encodeQuerySign qs =
          let ceq = Blaze8.fromChar '='
              cqt = Blaze8.fromChar '?'
              camp = Blaze8.fromChar '&'
              overrideParams = map B8.pack ["response-content-type", "response-content-language", "response-expires", "response-cache-control", "response-content-disposition", "response-content-encoding"]
              encItem (k, mv) =
                  let enc = if k `elem` overrideParams then Blaze.copyByteString else HTTP.urlEncodeBuilder True
                  in  enc k `mappend` maybe mempty (mappend ceq . enc) mv
          in case intersperse camp (map encItem qs) of
               [] -> mempty
               qs' -> mconcat (cqt :qs')

      ti = case (s3UseUri, signatureTimeInfo) of
             (False, ti') -> ti'
             (True, AbsoluteTimestamp time) -> AbsoluteExpires $ s3DefaultExpiry `addUTCTime` time
             (True, AbsoluteExpires time) -> AbsoluteExpires time
      sig = signature signatureCredentials HmacSHA1 stringToSign
      iamTok = maybe [] (\x -> [("x-amz-security-token", x)]) (iamToken signatureCredentials)
      stringToSign = Blaze.toByteString . mconcat . intersperse (Blaze8.fromChar '\n') . concat  $
                       [[Blaze.copyByteString $ httpMethod s3QMethod]
                       , [maybe mempty (Blaze.copyByteString . Base64.encode . ByteArray.convert) s3QContentMd5]
                       , [maybe mempty Blaze.copyByteString s3QContentType]
                       , [Blaze.copyByteString $ case ti of
                                                   AbsoluteTimestamp time -> fmtRfc822Time time
                                                   AbsoluteExpires time -> fmtTimeEpochSeconds time]
                       , map amzHeader amzHeaders
                       , [canonicalizedResource]
                       ]
          where amzHeader (k, v) = Blaze.copyByteString (CI.foldedCase k) `mappend` Blaze8.fromChar ':' `mappend` Blaze.copyByteString v
      (authorization, authQuery) = case ti of
                                 AbsoluteTimestamp _
                                        | isanon -> (Nothing, [])
                                        | otherwise -> (Just $ return $ B.concat ["AWS ", accessKeyID signatureCredentials, ":", sig], [])
                                 AbsoluteExpires time -> (Nothing, HTTP.toQuery $ makeAuthQuery time)
      makeAuthQuery time
        | isanon = []
        | otherwise = 
                [ ("Expires" :: B8.ByteString, fmtTimeEpochSeconds time)
                , ("AWSAccessKeyId", accessKeyID signatureCredentials)
                , ("SignatureMethod", "HmacSHA256")
                , ("Signature", sig)] ++ iamTok
s3SignQuery sq@S3Query{..} sc@S3Configuration{ s3SignVersion = S3SignV4 signpayload, .. } sd@SignatureData{..}
    | isAnonymousCredentials signatureCredentials =
      s3SignQuery sq (sc { s3SignVersion = S3SignV2 }) sd
    | otherwise = SignedQuery
      { sqMethod = s3QMethod
      , sqProtocol = s3Protocol
      , sqHost = B.intercalate "." $ catMaybes host
      , sqPort = s3Port
      , sqPath = mconcat $ catMaybes path
      , sqQuery = queryString ++ signatureQuery :: HTTP.Query
      , sqDate = Just signatureTime
      , sqAuthorization = authorization
      , sqContentType = s3QContentType
      , sqContentMd5 = s3QContentMd5
      , sqAmzHeaders = Map.toList amzHeaders
      , sqOtherHeaders = s3QOtherHeaders
      , sqBody = s3QRequestBody
      , sqStringToSign = stringToSign
      }
    where
        -- V4 signing
        -- * <http://docs.aws.amazon.com/general/latest/gr/sigv4_signing.html>
        -- * <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-auth-using-authorization-header.html>
        -- * <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html>

        iamTok = maybe [] (\x -> [(hAmzSecurityToken, x)]) $ iamToken signatureCredentials

        amzHeaders = Map.fromList $ (hAmzDate, sigTime):(hAmzContentSha256, payloadHash):iamTok ++ s3QAmzHeaders
            where
                -- needs to match the one produces in the @authorizationV4@
                sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime
                payloadHash = case (signpayload, s3QRequestBody) of
                    (AlwaysUnsigned, _)                 -> "UNSIGNED-PAYLOAD"
                    (_, Nothing)                        -> emptyBodyHash
                    (_, Just (HTTP.RequestBodyLBS lbs)) -> Base16.encode $ ByteArray.convert (CH.hashlazy lbs :: CH.Digest CH.SHA256)
                    (_, Just (HTTP.RequestBodyBS bs))   -> Base16.encode $ ByteArray.convert (CH.hash bs :: CH.Digest CH.SHA256)
                    (SignWithEffort, _)                 -> "UNSIGNED-PAYLOAD"
                    (AlwaysSigned, _)                   -> error "aws: RequestBody must be a on-memory one when AlwaysSigned mode."
                emptyBodyHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

        (host, path) = case s3RequestStyle of
            PathStyle   -> ([Just s3Endpoint], [Just "/", fmap (`B8.snoc` '/') s3QBucket, urlEncodedS3QObject])
            BucketStyle -> ([s3QBucket, Just s3Endpoint], [Just "/", urlEncodedS3QObject])
            VHostStyle  -> ([Just $ fromMaybe s3Endpoint s3QBucket], [Just "/", urlEncodedS3QObject])
            where
                urlEncodedS3QObject = s3UriEncode False <$> s3QObject

        -- must provide host in the canonical headers.
        canonicalHeaders = Map.union amzHeaders . Map.fromList $ catMaybes
            [ Just ("host", B.intercalate "." $ catMaybes host)
            , ("content-type",) <$> s3QContentType
            ]
        signedHeaders = B8.intercalate ";" (map CI.foldedCase $ Map.keys canonicalHeaders)
        stringToSign = B.intercalate "\n" $
            [ httpMethod s3QMethod                   -- method
            , mconcat . catMaybes $ path             -- path
            , s3RenderQuery False $ sort queryString -- query string
            ] ++
            Map.foldMapWithKey (\a b -> [CI.foldedCase a Sem.<> ":" Sem.<> b]) canonicalHeaders ++
            [ "" -- end headers
            , signedHeaders
            , amzHeaders Map.! hAmzContentSha256
            ]

        (authorization, signatureQuery, queryString) = case ti of
            AbsoluteTimestamp _  -> (Just auth, [], allQueries)
            AbsoluteExpires time ->
                ( Nothing
                , [(CI.original hAmzSignature, Just sig)]
                , (allQueries ++) . HTTP.toQuery . map (first CI.original) $
                    [ (hAmzAlgorithm, "AWS4-HMAC-SHA256")
                    , (hAmzCredential, cred)
                    , (hAmzDate, amzHeaders Map.! hAmzDate)
                    , (hAmzExpires, B8.pack . (show :: Integer -> String) . floor $ diffUTCTime time signatureTime)
                    , (hAmzSignedHeaders, signedHeaders)
                    ] ++ iamTok
                )
            where
                allQueries = s3QSubresources ++ s3QQuery
                region = s3ExtractRegion s3Endpoint
                auth = authorizationV4 sd HmacSHA256 region "s3" signedHeaders stringToSign
                sig  = signatureV4     sd HmacSHA256 region "s3"               stringToSign
                cred = credentialV4    sd            region "s3"
                ti = case (s3UseUri, signatureTimeInfo) of
                    (False, t) -> t
                    (True, AbsoluteTimestamp time) -> AbsoluteExpires $ s3DefaultExpiry `addUTCTime` time
                    (True, AbsoluteExpires time) -> AbsoluteExpires time

-- | Custom UriEncode function
-- see <http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html>
s3UriEncode
    :: Bool         -- ^ Whether encode slash characters
    -> B.ByteString
    -> B.ByteString
s3UriEncode encodeSlash = B8.concatMap $ \c ->
    if (isAscii c && isAlphaNum c) || (c `elem` nonEncodeMarks)
        then B8.singleton c
        else B8.pack $ '%' : map toUpper (showHex (ord c) "")
    where
        nonEncodeMarks :: String
        nonEncodeMarks = if encodeSlash
            then "_-~."
            else "_-~./"

s3RenderQuery
    :: Bool -- ^ Whether prepend a question mark
    -> HTTP.Query
    -> B.ByteString
s3RenderQuery qm = mconcat . qmf . intersperse (B8.singleton '&') . map renderItem
    where
        qmf = if qm then ("?":) else id

        renderItem :: HTTP.QueryItem -> B8.ByteString
        renderItem (k, Just v) = s3UriEncode True k Sem.<> "=" Sem.<> s3UriEncode True v
        renderItem (k, Nothing) = s3UriEncode True k Sem.<> "="

-- | see: <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region>
s3ExtractRegion :: B.ByteString -> B.ByteString
s3ExtractRegion "s3.amazonaws.com"            = "us-east-1"
s3ExtractRegion "s3-external-1.amazonaws.com" = "us-east-1"
s3ExtractRegion domain = either (const domain) B.pack $ Atto.parseOnly parser domain
    where
        -- s3.dualstack.<WA-DIR-N>.amazonaws.com
        -- s3-<WA-DIR-N>.amazonaws.com
        -- s3.<WA-DIR-N>.amazonaws.com
        parser = do
            _ <- Atto.string "s3"
            _ <- Atto.string ".dualstack." <|> Atto.string "-" <|> Atto.string "."
            r <- Atto.manyTill Atto.anyWord8 $ Atto.string ".amazonaws.com"
            Atto.endOfInput
            return r

s3ResponseConsumer :: HTTPResponseConsumer a
                         -> IORef S3Metadata
                         -> HTTPResponseConsumer a
s3ResponseConsumer inner metadataRef = s3BinaryResponseConsumer inner' metadataRef
  where inner' resp =
          do
            !res <- inner resp
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
    = do doc <- C.runConduit $ HTTP.responseBody resp .| XML.sinkDoc XML.def
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
                               endpoint = T.encodeUtf8 <$> (T.stripPrefix (fromMaybe "" bucket Sem.<> ".") =<< endpointRaw)
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
      , userDisplayName :: Maybe T.Text
      }
    deriving (Show)

parseUserInfo :: MonadThrow m => Cu.Cursor -> m UserInfo
parseUserInfo el = do id_ <- force "Missing user ID" $ el $/ elContent "ID"
                      displayName <- return $ case (el $/ elContent "DisplayName") of
                                                  (x:_) -> Just x
                                                  []    -> Nothing
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

data ObjectVersionInfo
    = ObjectVersion {
        oviKey          :: T.Text
      , oviVersionId    :: T.Text
      , oviIsLatest     :: Bool
      , oviLastModified :: UTCTime
      , oviETag         :: T.Text
      , oviSize         :: Integer
      , oviStorageClass :: StorageClass
      , oviOwner        :: Maybe UserInfo
      }
    | DeleteMarker {
        oviKey          :: T.Text
      , oviVersionId    :: T.Text
      , oviIsLatest     :: Bool
      , oviLastModified :: UTCTime
      , oviOwner        :: Maybe UserInfo
      }
    deriving (Show)

parseObjectVersionInfo :: MonadThrow m => Cu.Cursor -> m ObjectVersionInfo
parseObjectVersionInfo el
    = do key <- force "Missing object Key" $ el $/ elContent "Key"
         versionId <- force "Missing object VersionId" $ el $/ elContent "VersionId"
         isLatest <- forceM "Missing object IsLatest" $ el $/ elContent "IsLatest" &| textReadBool
         let time s = case (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ T.unpack s) <|>
                           (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" $ T.unpack s) of
                        Nothing -> throwM $ XmlException "Invalid time"
                        Just v -> return v
         lastModified <- forceM "Missing object LastModified" $ el $/ elContent "LastModified" &| time
         owner <- case el $/ Cu.laxElement "Owner" &| parseUserInfo of
                    (x:_) -> fmap' Just x
                    [] -> return Nothing
         case Cu.node el of
           XML.NodeElement e | elName e == "Version" ->
             do eTag <- force "Missing object ETag" $ el $/ elContent "ETag"
                size <- forceM "Missing object Size" $ el $/ elContent "Size" &| textReadInt
                storageClass <- forceM "Missing object StorageClass" $ el $/ elContent "StorageClass" &| return . parseStorageClass
                return ObjectVersion{
                             oviKey          = key
                           , oviVersionId    = versionId
                           , oviIsLatest     = isLatest
                           , oviLastModified = lastModified
                           , oviETag         = eTag
                           , oviSize         = size
                           , oviStorageClass = storageClass
                           , oviOwner        = owner
                           }
           XML.NodeElement e | elName e == "DeleteMarker" ->
             return DeleteMarker{
                             oviKey          = key
                           , oviVersionId    = versionId
                           , oviIsLatest     = isLatest
                           , oviLastModified = lastModified
                           , oviOwner        = owner
                           }
           _ -> throwM $ XmlException "Invalid object version tag"
    where
      elName = XML.nameLocalName . XML.elementName
      fmap' :: Monad m => (a -> b) -> m a -> m b
      fmap' f ma = ma >>= return . f

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
         let time s = case (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" $ T.unpack s) <|>
                           (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" $ T.unpack s) of
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

locationUsClassic, locationUsWest, locationUsWest2, locationEu, locationEuWest2, locationEuFrankfurt, locationApSouthEast, locationApSouthEast2, locationApNorthEast, locationSA :: LocationConstraint
locationUsClassic = ""
locationUsWest = "us-west-1"
locationUsWest2 = "us-west-2"
locationEu = "EU"
locationEuWest2 = "eu-west-2"
locationEuFrankfurt = "eu-central-1"
locationApSouthEast = "ap-southeast-1"
locationApSouthEast2 = "ap-southeast-2"
locationApNorthEast = "ap-northeast-1"
locationSA = "sa-east-1"

normaliseLocation :: LocationConstraint -> LocationConstraint
normaliseLocation location
  | location == "eu-west-1" = locationEu
  | otherwise = location
