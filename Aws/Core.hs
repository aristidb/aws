{-# LANGUAGE CPP #-}
module Aws.Core
( -- * Logging
  Loggable(..)
  -- * Response
  -- ** Metadata in responses
, Response(..)
, readResponse
, readResponseIO
, tellMetadata
, tellMetadataRef
, mapMetadata
  -- ** Response data consumers
, HTTPResponseConsumer
, ResponseConsumer(..)
  -- ** Memory response
, AsMemoryResponse(..)
  -- ** List response
, ListResponse(..)
  -- ** Exception types
, XmlException(..)
, HeaderException(..)
, FormException(..)
, NoCredentialsException(..)
  -- ** Response deconstruction helpers
, readHex2
  -- *** XML
, elContent
, elCont
, force
, forceM
, textReadInt
, readInt
, xmlCursorConsumer
  -- * Query
, SignedQuery(..)
, NormalQuery
, UriOnlyQuery
, queryToHttpRequest
, queryToUri
  -- ** Expiration
, TimeInfo(..)
, AbsoluteTimeInfo(..)
, fromAbsoluteTimeInfo
, makeAbsoluteTimeInfo
 -- ** Signature
, SignatureData(..)
, signatureData
, SignQuery(..)
, AuthorizationHash(..)
, amzHash
, signature
, authorizationV4
  -- ** Query construction helpers
, queryList
, awsBool
, awsTrue
, awsFalse
, fmtTime
, fmtRfc822Time
, rfc822Time
, fmtAmzTime
, fmtTimeEpochSeconds
, parseHttpDate
, httpDate1
, textHttpDate
, iso8601UtcDate
  -- * Transactions
, Transaction
, IteratedTransaction(..)
  -- * Credentials
, Credentials(..)
, makeCredentials
, credentialsDefaultFile
, credentialsDefaultKey
, loadCredentialsFromFile
, loadCredentialsFromEnv
, loadCredentialsFromInstanceMetadata
, loadCredentialsFromEnvOrFile
, loadCredentialsFromEnvOrFileOrInstanceMetadata
, loadCredentialsDefault
  -- * Service configuration
, DefaultServiceConfiguration(..)
  -- * HTTP types
, Protocol(..)
, defaultPort
, Method(..)
, httpMethod
)
where

import           Aws.Ec2.InstanceMetadata
import           Aws.Network
import qualified Blaze.ByteString.Builder as Blaze
import           Control.Applicative
import           Control.Arrow
import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (ResourceT, MonadThrow (throwM))
import           Crypto.Hash
import qualified Data.Aeson               as A
import           Data.Byteable
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Base16   as Base16
import qualified Data.ByteString.Base64   as Base64
import           Data.ByteString.Char8    ({- IsString -})
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.UTF8     as BU
import           Data.Char
import           Data.Conduit             (($$+-))
import qualified Data.Conduit             as C
import qualified Data.Conduit.List        as CL
import           Data.Default             (def)
import           Data.IORef
import           Data.List
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import           Data.Time
import qualified Data.Traversable         as Traversable
import           Data.Typeable
import           Data.Word
import qualified Network.HTTP.Conduit     as HTTP
import qualified Network.HTTP.Types       as HTTP
import           System.Directory
import           System.Environment
import           System.FilePath          ((</>))
import           System.Locale
import qualified Text.XML                 as XML
import qualified Text.XML.Cursor          as Cu
import           Text.XML.Cursor          hiding (force, forceM)
-------------------------------------------------------------------------------

-- | Types that can be logged (textually).
class Loggable a where
    toLogText :: a -> T.Text

-- | A response with metadata. Can also contain an error response, or
-- an internal error, via 'Attempt'.
--
-- Response forms a Writer-like monad.
data Response m a = Response { responseMetadata :: m
                             , responseResult :: Either E.SomeException a }
    deriving (Show, Functor)

-- | Read a response result (if it's a success response, fail otherwise).
readResponse :: MonadThrow n => Response m a -> n a
readResponse = either throwM return . responseResult

-- | Read a response result (if it's a success response, fail otherwise). In MonadIO.
readResponseIO :: MonadIO io => Response m a -> io a
readResponseIO = liftIO . readResponse

-- | An empty response with some metadata.
tellMetadata :: m -> Response m ()
tellMetadata m = Response m (return ())

-- | Apply a function to the metadata.
mapMetadata :: (m -> n) -> Response m a -> Response n a
mapMetadata f (Response m a) = Response (f m) a

--multiResponse :: Monoid m => Response m a -> Response [m] a ->

instance Monoid m => Applicative (Response m) where
    pure x = Response mempty (Right x)
    (<*>) = ap

instance Monoid m => Monad (Response m) where
    return x = Response mempty (Right x)
    Response m1 (Left e) >>= _ = Response m1 (Left e)
    Response m1 (Right x) >>= f = let Response m2 y = f x
                                  in Response (m1 `mappend` m2) y -- currently using First-semantics, Last SHOULD work too

instance Monoid m => MonadThrow (Response m) where
    throwM e = Response mempty (throwM e)

-- | Add metadata to an 'IORef' (using 'mappend').
tellMetadataRef :: Monoid m => IORef m -> m -> IO ()
tellMetadataRef r m = modifyIORef r (`mappend` m)

-- | A full HTTP response parser. Takes HTTP status, response headers, and response body.
type HTTPResponseConsumer a = HTTP.Response (C.ResumableSource (ResourceT IO) ByteString)
                              -> ResourceT IO a

-- | Class for types that AWS HTTP responses can be parsed into.
--
-- The request is also passed for possibly required additional metadata.
--
-- Note that for debugging, there is an instance for 'L.ByteString'.
class Monoid (ResponseMetadata resp) => ResponseConsumer req resp where
    -- | Metadata associated with a response. Typically there is one
    -- metadata type for each AWS service.
    type ResponseMetadata resp

    -- | Response parser. Takes the corresponding request, an 'IORef'
    -- for metadata, and HTTP response data.
    responseConsumer :: req -> IORef (ResponseMetadata resp) -> HTTPResponseConsumer resp

-- | Does not parse response. For debugging.
instance ResponseConsumer r (HTTP.Response L.ByteString) where
    type ResponseMetadata (HTTP.Response L.ByteString) = ()
    responseConsumer _ _ resp = do
        bss <- HTTP.responseBody resp $$+- CL.consume
        return resp
            { HTTP.responseBody = L.fromChunks bss
            }

-- | Class for responses that are fully loaded into memory
class AsMemoryResponse resp where
    type MemoryResponse resp :: *
    loadToMemory :: resp -> ResourceT IO (MemoryResponse resp)

-- | Responses that have one main list in them, and perhaps some decoration.
class ListResponse resp item | resp -> item where
    listResponse :: resp -> [item]


-- | Associates a request type and a response type in a bi-directional way.
--
-- This allows the type-checker to infer the response type when given
-- the request type and vice versa.
--
-- Note that the actual request generation and response parsing
-- resides in 'SignQuery' and 'ResponseConsumer' respectively.
class (SignQuery r, ResponseConsumer r a, Loggable (ResponseMetadata a))
      => Transaction r a
      | r -> a

-- | A transaction that may need to be split over multiple requests, for example because of upstream response size limits.
class Transaction r a => IteratedTransaction r a | r -> a where
    nextIteratedRequest :: r -> a -> Maybe r

-- | Signature version 4: ((region, service),(date,key))
type V4Key = ((B.ByteString,B.ByteString),(B.ByteString,B.ByteString))

-- | AWS access credentials.
data Credentials
    = Credentials {
        -- | AWS Access Key ID.
        accessKeyID :: B.ByteString
        -- | AWS Secret Access Key.
      , secretAccessKey :: B.ByteString
        -- | Signing keys for signature version 4
      , v4SigningKeys :: IORef [V4Key]
        -- | Signed IAM token
      , iamToken :: Maybe B.ByteString
      }
instance Show Credentials where
    show c = "Credentials{accessKeyID=" ++ show (accessKeyID c) ++ ",secretAccessKey=" ++ show (secretAccessKey c) ++ ",iamToken=" ++ show (iamToken c) ++ "}"

makeCredentials :: MonadIO io
                => B.ByteString -- ^ AWS Access Key ID
                -> B.ByteString -- ^ AWS Secret Access Key
                -> io Credentials
makeCredentials accessKeyID secretAccessKey = liftIO $ do
    v4SigningKeys <- newIORef []
    let iamToken = Nothing
    return Credentials { .. }

-- | The file where access credentials are loaded, when using 'loadCredentialsDefault'.
--
-- Value: /<user directory>/@/.aws-keys@
credentialsDefaultFile :: MonadIO io => io FilePath
credentialsDefaultFile = liftIO $ (</> ".aws-keys") <$> getHomeDirectory

-- | The key to be used in the access credential file that is loaded, when using 'loadCredentialsDefault'.
--
-- Value: @default@
credentialsDefaultKey :: T.Text
credentialsDefaultKey = "default"

-- | Load credentials from a (text) file given a key name.
--
-- The file consists of a sequence of lines, each in the following format:
--
-- @keyName awsKeyID awsKeySecret@
loadCredentialsFromFile :: MonadIO io => FilePath -> T.Text -> io (Maybe Credentials)
loadCredentialsFromFile file key = liftIO $ do
  exists <- doesFileExist file
  if exists
    then do
      contents <- map T.words . T.lines <$> T.readFile file
      Traversable.sequence $ do
        [_key, keyID, secret] <- find (hasKey key) contents
        return (makeCredentials (T.encodeUtf8 keyID) (T.encodeUtf8 secret))
    else return Nothing
  where
    hasKey _ [] = False
    hasKey k (k2 : _) = k == k2

-- | Load credentials from the environment variables @AWS_ACCESS_KEY_ID@ and @AWS_ACCESS_KEY_SECRET@
--   (or @AWS_SECRET_ACCESS_KEY@), if possible.
loadCredentialsFromEnv :: MonadIO io => io (Maybe Credentials)
loadCredentialsFromEnv = liftIO $ do
  env <- getEnvironment
  let lk = flip lookup env
      keyID = lk "AWS_ACCESS_KEY_ID"
      secret = lk "AWS_ACCESS_KEY_SECRET" `mplus` lk "AWS_SECRET_ACCESS_KEY"
  Traversable.sequence
      (makeCredentials <$> (T.encodeUtf8 . T.pack <$> keyID)
                       <*> (T.encodeUtf8 . T.pack <$> secret))

loadCredentialsFromInstanceMetadata :: MonadIO io => io (Maybe Credentials)
loadCredentialsFromInstanceMetadata = liftIO $ HTTP.withManager $ \mgr ->
  do
    -- check if the path is routable
    avail <- liftIO $ hostAvailable "169.254.169.254"
    if not avail
      then return Nothing
      else do
        info <- liftIO $ E.catch (getInstanceMetadata mgr "latest/meta-data/iam" "info" >>= return . Just) (\(_ :: HTTP.HttpException) -> return Nothing)
        let infodict = info >>= A.decode :: Maybe (M.Map String String)
            info'    = infodict >>= M.lookup "InstanceProfileArn"
        case info' of
          Just name ->
            do
              let name' = drop 1 $ dropWhile (/= '/') $ name
              creds <- liftIO $ E.catch (getInstanceMetadata mgr "latest/meta-data/iam/security-credentials" name' >>= return . Just) (\(_ :: HTTP.HttpException) -> return Nothing)
              -- this token lasts ~6 hours
              let dict   = creds >>= A.decode :: Maybe (M.Map String String)
                  keyID  = dict  >>= M.lookup "AccessKeyId"
                  secret = dict  >>= M.lookup "SecretAccessKey"
                  token  = dict  >>= M.lookup "Token"
              ref <- liftIO $ newIORef []
              return (Credentials <$> (T.encodeUtf8 . T.pack <$> keyID)
                                  <*> (T.encodeUtf8 . T.pack <$> secret)
                                  <*> return ref
                                  <*> (Just . T.encodeUtf8 . T.pack <$> token))
          Nothing -> return Nothing

-- | Load credentials from environment variables if possible, or alternatively from a file with a given key name.
--
-- See 'loadCredentialsFromEnv' and 'loadCredentialsFromFile' for details.
loadCredentialsFromEnvOrFile :: MonadIO io => FilePath -> T.Text -> io (Maybe Credentials)
loadCredentialsFromEnvOrFile file key =
  do
    envcr <- loadCredentialsFromEnv
    case envcr of
      Just cr -> return (Just cr)
      Nothing -> loadCredentialsFromFile file key

-- | Load credentials from environment variables if possible, or alternatively from the instance metadata store, or alternatively from a file with a given key name.
--
-- See 'loadCredentialsFromEnv', 'loadCredentialsFromFile' and 'loadCredentialsFromInstanceMetadata' for details.
loadCredentialsFromEnvOrFileOrInstanceMetadata :: MonadIO io => FilePath -> T.Text -> io (Maybe Credentials)
loadCredentialsFromEnvOrFileOrInstanceMetadata file key =
  do
    envcr <- loadCredentialsFromEnv
    case envcr of
      Just cr -> return (Just cr)
      Nothing ->
        do
          filecr <- loadCredentialsFromFile file key
          case filecr of
            Just cr -> return (Just cr)
            Nothing -> loadCredentialsFromInstanceMetadata

-- | Load credentials from environment variables if possible, or alternative from the default file with the default
-- key name.
--
-- Default file: /<user directory>/@/.aws-keys@
-- Default key name: @default@
--
-- See 'loadCredentialsFromEnv' and 'loadCredentialsFromFile' for details.
loadCredentialsDefault :: MonadIO io => io (Maybe Credentials)
loadCredentialsDefault = do
  file <- credentialsDefaultFile
  loadCredentialsFromEnvOrFileOrInstanceMetadata file credentialsDefaultKey

-- | Protocols supported by AWS. Currently, all AWS services use the HTTP or HTTPS protocols.
data Protocol
    = HTTP
    | HTTPS
    deriving (Eq,Read,Show,Ord,Typeable)

-- | The default port to be used for a protocol if no specific port is specified.
defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443

-- | Request method. Not all request methods are supported by all services.
data Method
    = Head      -- ^ HEAD method. Put all request parameters in a query string and HTTP headers.
    | Get       -- ^ GET method. Put all request parameters in a query string and HTTP headers.
    | PostQuery -- ^ POST method. Put all request parameters in a query string and HTTP headers, but send the query string
                --   as a POST payload
    | Post      -- ^ POST method. Sends a service- and request-specific request body.
    | Put       -- ^ PUT method.
    | Delete    -- ^ DELETE method.
    deriving (Show, Eq)

-- | HTTP method associated with a request method.
httpMethod :: Method -> HTTP.Method
httpMethod Head      = "HEAD"
httpMethod Get       = "GET"
httpMethod PostQuery = "POST"
httpMethod Post      = "POST"
httpMethod Put       = "PUT"
httpMethod Delete    = "DELETE"

-- | A pre-signed medium-level request object.
data SignedQuery
    = SignedQuery {
        -- | Request method.
        sqMethod :: !Method
        -- | Protocol to be used.
      , sqProtocol :: !Protocol
        -- | HTTP host.
      , sqHost :: !B.ByteString
        -- | IP port.
      , sqPort :: !Int
        -- | HTTP path.
      , sqPath :: !B.ByteString
        -- | Query string list (used with 'Get' and 'PostQuery').
      , sqQuery :: !HTTP.Query
        -- | Request date/time.
      , sqDate :: !(Maybe UTCTime)
        -- | Authorization string (if applicable), for @Authorization@ header.  See 'authorizationV4'
      , sqAuthorization :: !(Maybe (IO B.ByteString))
        -- | Request body content type.
      , sqContentType :: !(Maybe B.ByteString)
        -- | Request body content MD5.
      , sqContentMd5 :: !(Maybe (Digest MD5))
        -- | Additional Amazon "amz" headers.
      , sqAmzHeaders :: !HTTP.RequestHeaders
        -- | Additional non-"amz" headers.
      , sqOtherHeaders :: !HTTP.RequestHeaders
        -- | Request body (used with 'Post' and 'Put').
#if MIN_VERSION_http_conduit(2, 0, 0)
      , sqBody :: !(Maybe HTTP.RequestBody)
#else
      , sqBody :: !(Maybe (HTTP.RequestBody (C.ResourceT IO)))
#endif
        -- | String to sign. Note that the string is already signed, this is passed mostly for debugging purposes.
      , sqStringToSign :: !B.ByteString
      }
    --deriving (Show)

-- | Create a HTTP request from a 'SignedQuery' object.
#if MIN_VERSION_http_conduit(2, 0, 0)
queryToHttpRequest :: SignedQuery -> IO HTTP.Request
#else
queryToHttpRequest :: SignedQuery -> IO (HTTP.Request (C.ResourceT IO))
#endif
queryToHttpRequest SignedQuery{..} =  do
    mauth <- maybe (return Nothing) (Just<$>) sqAuthorization
    return $ def {
        HTTP.method = httpMethod sqMethod
      , HTTP.secure = case sqProtocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.host = sqHost
      , HTTP.port = sqPort
      , HTTP.path = sqPath
      , HTTP.queryString =
          if sqMethod == PostQuery
            then ""
            else HTTP.renderQuery False sqQuery

      , HTTP.requestHeaders = catMaybes [ checkDate (\d -> ("Date", fmtRfc822Time d)) sqDate
                                        , fmap (\c -> ("Content-Type", c)) contentType
                                        , fmap (\md5 -> ("Content-MD5", Base64.encode $ toBytes md5)) sqContentMd5
                                        , fmap (\auth -> ("Authorization", auth)) mauth]
                              ++ sqAmzHeaders
                              ++ sqOtherHeaders
      , HTTP.requestBody =

        -- An explicityly defined body parameter should overwrite everything else.
        case sqBody of
          Just x -> x
          Nothing ->
            -- a POST query should convert its query string into the body
            case sqMethod of
              PostQuery -> HTTP.RequestBodyLBS . Blaze.toLazyByteString $
                           HTTP.renderQueryBuilder False sqQuery
              _         -> HTTP.RequestBodyBuilder 0 mempty

      , HTTP.decompress = HTTP.alwaysDecompress
      , HTTP.checkStatus = \_ _ _ -> Nothing
      }
    where
      checkDate f mb = maybe (f <$> mb) (const Nothing) $ lookup "date" sqOtherHeaders
      -- An explicitly defined content-type should override everything else.
      contentType = sqContentType `mplus` defContentType
      defContentType = case sqMethod of
                         PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"
                         _ -> Nothing

-- | Create a URI fro a 'SignedQuery' object.
--
-- Unused / incompatible fields will be silently ignored.
queryToUri :: SignedQuery -> B.ByteString
queryToUri SignedQuery{..}
    = B.concat [
       case sqProtocol of
         HTTP -> "http://"
         HTTPS -> "https://"
      , sqHost
      , if sqPort == defaultPort sqProtocol then "" else T.encodeUtf8 . T.pack $ ':' : show sqPort
      , sqPath
      , HTTP.renderQuery True sqQuery
      ]

-- | Whether to restrict the signature validity with a plain timestamp, or with explicit expiration
-- (absolute or relative).
data TimeInfo
    = Timestamp                                      -- ^ Use a simple timestamp to let AWS check the request validity.
    | ExpiresAt { fromExpiresAt :: UTCTime }         -- ^ Let requests expire at a specific fixed time.
    | ExpiresIn { fromExpiresIn :: NominalDiffTime } -- ^ Let requests expire a specific number of seconds after they
                                                     -- were generated.
    deriving (Show)

-- | Like 'TimeInfo', but with all relative times replaced by absolute UTC.
data AbsoluteTimeInfo
    = AbsoluteTimestamp { fromAbsoluteTimestamp :: UTCTime }
    | AbsoluteExpires { fromAbsoluteExpires :: UTCTime }
    deriving (Show)

-- | Just the UTC time value.
fromAbsoluteTimeInfo :: AbsoluteTimeInfo -> UTCTime
fromAbsoluteTimeInfo (AbsoluteTimestamp time) = time
fromAbsoluteTimeInfo (AbsoluteExpires time) = time

-- | Convert 'TimeInfo' to 'AbsoluteTimeInfo' given the current UTC time.
makeAbsoluteTimeInfo :: TimeInfo -> UTCTime -> AbsoluteTimeInfo
makeAbsoluteTimeInfo Timestamp     now = AbsoluteTimestamp now
makeAbsoluteTimeInfo (ExpiresAt t) _   = AbsoluteExpires t
makeAbsoluteTimeInfo (ExpiresIn s) now = AbsoluteExpires $ addUTCTime s now

-- | Data that is always required for signing requests.
data SignatureData
    = SignatureData {
        -- | Expiration or timestamp.
        signatureTimeInfo :: AbsoluteTimeInfo
        -- | Current time.
      , signatureTime :: UTCTime
        -- | Access credentials.
      , signatureCredentials :: Credentials
      }

-- | Create signature data using the current system time.
signatureData :: TimeInfo -> Credentials -> IO SignatureData
signatureData rti cr = do
  now <- getCurrentTime
  let ti = makeAbsoluteTimeInfo rti now
  return SignatureData { signatureTimeInfo = ti, signatureTime = now, signatureCredentials = cr }

-- | Tag type for normal queries.
data NormalQuery
-- | Tag type for URI-only queries.
data UriOnlyQuery

-- | A "signable" request object. Assembles together the Query, and signs it in one go.
class SignQuery request where
    -- | Additional information, like API endpoints and service-specific preferences.
    type ServiceConfiguration request :: * {- Query Type -} -> *

    -- | Create a 'SignedQuery' from a request, additional 'Info', and 'SignatureData'.
    signQuery :: request -> ServiceConfiguration request queryType -> SignatureData -> SignedQuery

-- | Supported crypto hashes for the signature.
data AuthorizationHash
    = HmacSHA1
    | HmacSHA256
    deriving (Show)

-- | Authorization hash identifier as expected by Amazon.
amzHash :: AuthorizationHash -> B.ByteString
amzHash HmacSHA1 = "HmacSHA1"
amzHash HmacSHA256 = "HmacSHA256"

-- | Create a signature. Usually, AWS wants a specifically constructed string to be signed.
--
-- The signature is a HMAC-based hash of the string and the secret access key.
signature :: Credentials -> AuthorizationHash -> B.ByteString -> B.ByteString
signature cr ah input = Base64.encode sig
    where
      sig = case ah of
              HmacSHA1 -> computeSig SHA1
              HmacSHA256 -> computeSig SHA256
      computeSig :: HashAlgorithm a => a -> ByteString
      computeSig t = toBytes (hmacAlg t (secretAccessKey cr) input)

-- | Use this to create the Authorization header to set into 'sqAuthorization'.
-- See <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html>: you must create the
-- canonical request as explained by Step 1 and this function takes care of Steps 2 and 3.
authorizationV4 :: SignatureData
                -> AuthorizationHash
                -> B.ByteString -- ^ region, e.g. us-east-1
                -> B.ByteString -- ^ service, e.g. dynamodb
                -> B.ByteString -- ^ SignedHeaders, e.g. content-type;host;x-amz-date;x-amz-target
                -> B.ByteString -- ^ canonicalRequest (before hashing)
                -> IO B.ByteString
authorizationV4 sd ah region service headers canonicalRequest = do
    let ref = v4SigningKeys $ signatureCredentials sd
        date = fmtTime "%Y%m%d" $ signatureTime sd
        mkHmac k i = case ah of
                        HmacSHA1 -> toBytes (hmac k i :: HMAC SHA1)
                        HmacSHA256 -> toBytes (hmac k i :: HMAC SHA256)
        mkHash i = case ah of
                        HmacSHA1 -> toBytes (hash i :: Digest SHA1)
                        HmacSHA256 -> toBytes (hash i :: Digest SHA256)
        alg = case ah of
                    HmacSHA1 -> "AWS4-HMAC-SHA1"
                    HmacSHA256 -> "AWS4-HMAC-SHA256"

    -- Lookup existing signing key
    allkeys <- readIORef ref
    let mkey = case lookup (region,service) allkeys of
                Just (d,k) | d /= date -> Nothing
                           | otherwise -> Just k
                Nothing -> Nothing

    -- possibly create a new signing key
    key <- case mkey of
            Just k -> return k
            Nothing -> atomicModifyIORef ref $ \keylist ->
                            let secretKey = secretAccessKey $ signatureCredentials sd
                                kDate = mkHmac ("AWS4" <> secretKey) date
                                kRegion = mkHmac kDate region
                                kService = mkHmac kRegion service
                                kSigning = mkHmac kService "aws4_request"
                                lstK = (region,service)
                                keylist' = (lstK,(date,kSigning)) : filter ((lstK/=).fst) keylist
                             in (keylist', kSigning)

    -- now do the signature
    let canonicalRequestHash = Base16.encode $ mkHash canonicalRequest
        stringToSign = B.concat [ alg
                                , "\n"
                                , fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sd
                                , "\n"
                                , date
                                , "/"
                                , region
                                , "/"
                                , service
                                , "/aws4_request\n"
                                , canonicalRequestHash
                                ]
        sig = Base16.encode $ mkHmac key stringToSign

    -- finally, return the header
    return $ B.concat [ alg
                      , " Credential="
                      , accessKeyID (signatureCredentials sd)
                      , "/"
                      , date
                      , "/"
                      , region
                      , "/"
                      , service
                      , "/aws4_request,"
                      , "SignedHeaders="
                      , headers
                      , ",Signature="
                      , sig
                      ]

-- | Default configuration for a specific service.
class DefaultServiceConfiguration config where
    -- | Default service configuration.
    defServiceConfig :: config

    -- | Default debugging-only configuration. (Normally using HTTP instead of HTTPS for easier debugging.)
    debugServiceConfig :: config
    debugServiceConfig = defServiceConfig

-- | @queryList f prefix xs@ constructs a query list from a list of
-- elements @xs@, using a common prefix @prefix@, and a transformer
-- function @f@.
--
-- A dot (@.@) is interspersed between prefix and generated key.
--
-- Example:
--
-- @queryList swap \"pfx\" [(\"a\", \"b\"), (\"c\", \"d\")]@ evaluates to @[(\"pfx.b\", \"a\"), (\"pfx.d\", \"c\")]@
-- (except with ByteString instead of String, of course).
queryList :: (a -> [(B.ByteString, B.ByteString)]) -> B.ByteString -> [a] -> [(B.ByteString, B.ByteString)]
queryList f prefix xs = concat $ zipWith combine prefixList (map f xs)
    where prefixList = map (dot prefix . BU.fromString . show) [(1 :: Int) ..]
          combine pf = map $ first (pf `dot`)
          dot x y = B.concat [x, BU.fromString ".", y]

-- | A \"true\"/\"false\" boolean as requested by some services.
awsBool :: Bool -> B.ByteString
awsBool True = "true"
awsBool False = "false"

-- | \"true\"
awsTrue :: B.ByteString
awsTrue = awsBool True

-- | \"false\"
awsFalse :: B.ByteString
awsFalse = awsBool False

-- | Format time according to a format string, as a ByteString.
fmtTime :: String -> UTCTime -> B.ByteString
fmtTime s t = BU.fromString $ formatTime defaultTimeLocale s t

rfc822Time :: String
rfc822Time = "%a, %_d %b %Y %H:%M:%S GMT"

-- | Format time in RFC 822 format.
fmtRfc822Time :: UTCTime -> B.ByteString
fmtRfc822Time = fmtTime rfc822Time

-- | Format time in yyyy-mm-ddThh-mm-ss format.
fmtAmzTime :: UTCTime -> B.ByteString
fmtAmzTime = fmtTime "%Y-%m-%dT%H:%M:%S"

-- | Format time as seconds since the Unix epoch.
fmtTimeEpochSeconds :: UTCTime -> B.ByteString
fmtTimeEpochSeconds = fmtTime "%s"

-- | Parse HTTP-date (section 3.3.1 of RFC 2616)
parseHttpDate :: String -> Maybe UTCTime
parseHttpDate s =     p "%a, %d %b %Y %H:%M:%S GMT" s -- rfc1123-date
                  <|> p "%A, %d-%b-%y %H:%M:%S GMT" s -- rfc850-date
                  <|> p "%a %b %_d %H:%M:%S %Y" s     -- asctime-date
                  <|> p "%Y-%m-%dT%H:%M:%S%QZ" s      -- iso 8601
                  <|> p "%Y-%m-%dT%H:%M:%S%Q%Z" s     -- iso 8601
  where p = parseTime defaultTimeLocale

-- | HTTP-date (section 3.3.1 of RFC 2616, first type - RFC1123-style)
httpDate1 :: String
httpDate1 = "%a, %d %b %Y %H:%M:%S GMT" -- rfc1123-date

-- | Format (as Text) HTTP-date (section 3.3.1 of RFC 2616, first type - RFC1123-style)
textHttpDate :: UTCTime -> T.Text
textHttpDate = T.pack . formatTime defaultTimeLocale httpDate1

iso8601UtcDate :: String
iso8601UtcDate = "%Y-%m-%dT%H:%M:%S%QZ"

-- | Parse a two-digit hex number.
readHex2 :: [Char] -> Maybe Word8
readHex2 [c1,c2] = do n1 <- readHex1 c1
                      n2 <- readHex1 c2
                      return . fromIntegral $ n1 * 16 + n2
    where
      readHex1 c | c >= '0' && c <= '9' = Just $ ord c - ord '0'
                 | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
                 | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
      readHex1 _                        = Nothing
readHex2 _ = Nothing

-- XML

-- | An error that occurred during XML parsing / validation.
newtype XmlException = XmlException { xmlErrorMessage :: String }
    deriving (Show, Typeable)

instance E.Exception XmlException

-- | An error that occurred during header parsing / validation.
newtype HeaderException = HeaderException { headerErrorMessage :: String }
    deriving (Show, Typeable)

instance E.Exception HeaderException

-- | An error that occurred during form parsing / validation.
newtype FormException = FormException { formErrorMesage :: String }
    deriving (Show, Typeable)

instance E.Exception FormException

-- | No credentials were found and an invariant was violated.
newtype NoCredentialsException = NoCredentialsException { noCredentialsErrorMessage :: String }
    deriving (Show, Typeable)

instance E.Exception NoCredentialsException


-- | A specific element (case-insensitive, ignoring namespace - sadly necessary), extracting only the textual contents.
elContent :: T.Text -> Cursor -> [T.Text]
elContent name = laxElement name &/ content

-- | Like 'elContent', but extracts 'String's instead of 'T.Text'.
elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

-- | Extract the first element from a parser result list, and throw an 'XmlException' if the list is empty.
force :: MonadThrow m => String -> [a] -> m a
force = Cu.force . XmlException

-- | Extract the first element from a monadic parser result list, and throw an 'XmlException' if the list is empty.
forceM :: MonadThrow m => String -> [m a] -> m a
forceM = Cu.forceM . XmlException

-- | Read an integer from a 'T.Text', throwing an 'XmlException' on failure.
textReadInt :: (MonadThrow m, Num a) => T.Text -> m a
textReadInt s = case reads $ T.unpack s of
                  [(n,"")] -> return $ fromInteger n
                  _        -> throwM $ XmlException "Invalid Integer"

-- | Read an integer from a 'String', throwing an 'XmlException' on failure.
readInt :: (MonadThrow m, Num a) => String -> m a
readInt s = case reads s of
              [(n,"")] -> return $ fromInteger n
              _        -> throwM $ XmlException "Invalid Integer"

-- | Create a complete 'HTTPResponseConsumer' from a simple function that takes a 'Cu.Cursor' to XML in the response
-- body.
--
-- This function is highly recommended for any services that parse relatively short XML responses. (If status and response
-- headers are required, simply take them as function parameters, and pass them through to this function.)
xmlCursorConsumer ::
    (Monoid m)
    => (Cu.Cursor -> Response m a)
    -> IORef m
    -> HTTPResponseConsumer a
xmlCursorConsumer parse metadataRef res
    = do doc <- HTTP.responseBody res $$+- XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         let Response metadata x = parse cursor
         liftIO $ tellMetadataRef metadataRef metadata
         case x of
           Left err -> liftIO $ throwM err
           Right v  -> return v
