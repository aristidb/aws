{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, FlexibleContexts, FlexibleInstances, DeriveFunctor, OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
module Aws.Core
( -- * Response
  -- ** Metadata in responses
  Response(..)
, tellMetadata
, tellMetadataRef
  -- ** Response data consumers
, HTTPResponseConsumer
, ResponseConsumer(..)
  -- ** Exception types
, XmlException(..)
, HeaderException(..)
, FormException(..)
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
  -- * Transactions
, Transaction
  -- * Credentials
, Credentials(..)
, credentialsDefaultFile
, credentialsDefaultKey
, loadCredentialsFromFile
, loadCredentialsFromEnv
, loadCredentialsFromEnvOrFile
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

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attempt             (Attempt(..))
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    ({- IsString -})
import           Data.Char
import           Data.Conduit             (Source, ResourceT, ($$))
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.Typeable
import           Data.Word
import           System.Directory
import           System.Environment
import           System.FilePath          ((</>))
import           System.Locale
import           Text.XML.Cursor          hiding (force, forceM)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Control.Exception        as E
import qualified Control.Failure          as F
import qualified Crypto.Classes           as Crypto
import qualified Crypto.HMAC              as HMAC
import qualified Crypto.Hash.MD5          as MD5
import qualified Crypto.Hash.SHA1         as SHA1
import qualified Crypto.Hash.SHA256       as SHA256
import qualified Data.ByteString          as B
import qualified Data.ByteString.Base64   as Base64
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.UTF8     as BU
import qualified Data.Conduit             as C
import qualified Data.Conduit.List        as CL
import qualified Data.Serialize           as Serialize
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import qualified Network.HTTP.Conduit     as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Text.XML                 as XML
import qualified Text.XML.Cursor          as Cu

-- | A response with metadata. Can also contain an error response, or an internal error, via 'Attempt'.
-- 
-- Response forms a Writer-like monad.
data Response m a = Response m (Attempt a)
    deriving (Show, Functor)

-- | An empty response with some metadata.
tellMetadata :: m -> Response m ()
tellMetadata m = Response m (return ())

instance Monoid m => Monad (Response m) where
    return x = Response mempty (Success x)
    Response m1 (Failure e) >>= _ = Response m1 (Failure e)
    Response m1 (Success x) >>= f = let Response m2 y = f x
                                    in Response (m1 `mappend` m2) y -- currently using First-semantics, Last SHOULD work too

instance (Monoid m, E.Exception e) => F.Failure e (Response m) where
    failure e = Response mempty (F.failure e)

-- | Add metadata to an 'IORef' (using 'mappend').
tellMetadataRef :: Monoid m => IORef m -> m -> IO ()
tellMetadataRef r m = modifyIORef r (`mappend` m)

-- | A full HTTP response parser. Takes HTTP status, response headers, and response body.
type HTTPResponseConsumer a =  HTTP.Status
                            -> HTTP.ResponseHeaders
                            -> Source (ResourceT IO) ByteString
                            -> ResourceT IO a

-- | Class for types that AWS HTTP responses can be parsed into.
-- 
-- The request is also passed for possibly required additional metadata.
-- 
-- Note that for debugging, there is an instance for 'L.ByteString'.
class Monoid (ResponseMetadata resp) => ResponseConsumer req resp where
    -- | Metadata associated with a response. Typically there is one metadata type for each AWS service.
    type ResponseMetadata resp

    -- | Response parser. Takes the corresponding request, an 'IORef' for metadata, and HTTP response data.
    responseConsumer :: req -> IORef (ResponseMetadata resp) -> HTTPResponseConsumer resp

-- | Does not parse response. For debugging.
instance ResponseConsumer r (HTTP.Response L.ByteString) where
    type ResponseMetadata (HTTP.Response L.ByteString) = ()
    responseConsumer _ _ status headers bufsource = do
      chunks <- bufsource $$ CL.consume
      return (HTTP.Response status HTTP.http11 headers $ L.fromChunks chunks)

-- | Associates a request type and a response type in a bi-directional way.
-- 
-- This allows the type-checker to infer the response type when given the request type and vice versa.
-- 
-- Note that the actual request generation and response parsing resides in 'SignQuery' and 'ResponseConsumer'
-- respectively.
class (SignQuery r, ResponseConsumer r a)
      => Transaction r a
      | r -> a, a -> r

-- | AWS access credentials.
data Credentials
    = Credentials {
        -- | AWS Access Key ID.
        accessKeyID :: B.ByteString
        -- | AWS Secret Access Key.
      , secretAccessKey :: B.ByteString
      }
    deriving (Show)

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
  contents <- map T.words . T.lines <$> T.readFile file
  return $ do 
    [_key, keyID, secret] <- find (hasKey key) contents
    return Credentials { accessKeyID = T.encodeUtf8 keyID, secretAccessKey = T.encodeUtf8 secret }
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
  return (Credentials <$> (T.encodeUtf8 . T.pack <$> keyID) <*> (T.encodeUtf8 . T.pack <$> secret))

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
  loadCredentialsFromEnvOrFile file credentialsDefaultKey

-- | Protocols supported by AWS. Currently, all AWS services use the HTTP or HTTPS protocols.
data Protocol
    = HTTP
    | HTTPS
    deriving (Show)

-- | The default port to be used for a protocol if no specific port is specified.
defaultPort :: Protocol -> Int
defaultPort HTTP = 80
defaultPort HTTPS = 443

-- | Request method. Not all request methods are supported by all services.
data Method
    = Get       -- ^ GET method. Put all request parameters in a query string and HTTP headers.
    | PostQuery -- ^ POST method. Put all request parameters in a query string and HTTP headers, but send the query string
                --   as a POST payload
    | Post      -- ^ POST method. Sends a service- and request-specific request body.
    | Put       -- ^ PUT method.
    | Delete    -- ^ DELETE method.
    deriving (Show, Eq)

-- | HTTP method associated with a request method.
httpMethod :: Method -> HTTP.Method
httpMethod Get       = "GET"
httpMethod PostQuery = "POST"
httpMethod Post      = "POST"
httpMethod Put       = "PUT"
httpMethod Delete    = "DELETE"

-- | A pre-signed medium-level request object.
data SignedQuery
    = SignedQuery {
        -- | Request method.
        sqMethod :: Method
        -- | Protocol to be used.
      , sqProtocol :: Protocol
        -- | HTTP host.
      , sqHost :: B.ByteString
        -- | IP port.
      , sqPort :: Int
        -- | HTTP path.
      , sqPath :: B.ByteString
        -- | Query string list (used with 'Get' and 'PostQuery').
      , sqQuery :: HTTP.Query
        -- | Request date/time.
      , sqDate :: Maybe UTCTime
        -- | Authorization string (if applicable), for @Authorization@ header..
      , sqAuthorization :: Maybe B.ByteString
        -- | Request body content type.
      , sqContentType :: Maybe B.ByteString
        -- | Request body content MD5.
      , sqContentMd5 :: Maybe MD5.MD5
        -- | Additional Amazon "amz" headers.
      , sqAmzHeaders :: HTTP.RequestHeaders
        -- | Additional non-"amz" headers.
      , sqOtherHeaders :: HTTP.RequestHeaders
        -- | Request body (used with 'Post' and 'Put').
      , sqBody :: Maybe (HTTP.RequestBody (C.ResourceT IO))
        -- | String to sign. Note that the string is already signed, this is passed mostly for debugging purposes.
      , sqStringToSign :: B.ByteString
      }
    --deriving (Show)

-- | Create a HTTP request from a 'SignedQuery' object.
queryToHttpRequest :: SignedQuery -> HTTP.Request (C.ResourceT IO)
queryToHttpRequest SignedQuery{..}
    = HTTP.def {
        HTTP.method = httpMethod sqMethod
      , HTTP.secure = case sqProtocol of
                        HTTP -> False
                        HTTPS -> True
      , HTTP.host = sqHost
      , HTTP.port = sqPort
      , HTTP.path = sqPath
      , HTTP.queryString = HTTP.renderQuery False sqQuery
      , HTTP.requestHeaders = catMaybes [fmap (\d -> ("Date", fmtRfc822Time d)) sqDate
                                        , fmap (\c -> ("Content-Type", c)) contentType
                                        , fmap (\md5 -> ("Content-MD5", Serialize.encode md5)) sqContentMd5
                                        , fmap (\auth -> ("Authorization", auth)) sqAuthorization]
                              ++ sqAmzHeaders
                              ++ sqOtherHeaders
      , HTTP.requestBody = case sqMethod of
                             PostQuery -> HTTP.RequestBodyLBS . Blaze.toLazyByteString $ HTTP.renderQueryBuilder False sqQuery
                             _         -> case sqBody of
                                            Nothing -> HTTP.RequestBodyBuilder 0 mempty
                                            Just x  -> x
      , HTTP.decompress = HTTP.alwaysDecompress
      , HTTP.checkStatus = \_ _ -> Nothing
      }
    where contentType = case sqMethod of
                           PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"
                           _ -> sqContentType

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

-- | A "signable" request object. Assembles together the Query, and signs it in one go.
class SignQuery r where
    -- | Additional information, like API endpoints and service-specific preferences.
    type ServiceConfiguration r :: *
    
    -- | Create a 'SignedQuery' from a request, additional 'Info', and 'SignatureData'.
    signQuery :: r -> ServiceConfiguration r -> SignatureData -> SignedQuery

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
              HmacSHA1 -> computeSig (undefined :: SHA1.SHA1)
              HmacSHA256 -> computeSig (undefined :: SHA256.SHA256)
      computeSig :: Crypto.Hash c d => d -> B.ByteString
      computeSig t = Serialize.encode (HMAC.hmac' key input `asTypeOf` t)
      key :: HMAC.MacKey c d
      key = HMAC.MacKey (secretAccessKey cr)

-- | Default configuration for a specific service.
class DefaultServiceConfiguration config where
    -- | Default service configuration for normal requests.
    defaultConfiguration :: config
    
    -- | Default service configuration for URI-only requests.
    defaultConfigurationUri :: config

    -- | Default debugging-only configuration for normal requests. (Normally using HTTP instead of HTTPS for easier debugging.)
    debugConfiguration :: config
    debugConfiguration = defaultConfiguration

    -- | Default debugging-only configuration for URI-only requests. (Normally using HTTP instead of HTTPS for easier debugging.)
    debugConfigurationUri :: config
    debugConfigurationUri = defaultConfigurationUri

-- | @queryList f prefix xs@ constructs a query list from a list of elements @xs@, using a common prefix @prefix@,
-- and a transformer function @f@.
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
newtype FormException  = FormException { formErrorMesage :: String }
    deriving (Show, Typeable)

instance E.Exception FormException


-- | A specific element (case-insensitive, ignoring namespace - sadly necessary), extracting only the textual contents.
elContent :: T.Text -> Cursor -> [T.Text]
elContent name = laxElement name &/ content

-- | Like 'elContent', but extracts 'String's instead of 'T.Text'.
elCont :: T.Text -> Cursor -> [String]
elCont name = laxElement name &/ content &| T.unpack

-- | Extract the first element from a parser result list, and throw an 'XmlException' if the list is empty.
force :: F.Failure XmlException m => String -> [a] -> m a
force = Cu.force . XmlException

-- | Extract the first element from a monadic parser result list, and throw an 'XmlException' if the list is empty.
forceM :: F.Failure XmlException m => String -> [m a] -> m a
forceM = Cu.forceM . XmlException

-- | Read an integer from a 'T.Text', throwing an 'XmlException' on failure.
textReadInt :: (F.Failure XmlException m, Num a) => T.Text -> m a
textReadInt s = case reads $ T.unpack s of
                  [(n,"")] -> return $ fromInteger n
                  _        -> F.failure $ XmlException "Invalid Integer"

-- | Read an integer from a 'String', throwing an 'XmlException' on failure.
readInt :: (F.Failure XmlException m, Num a) => String -> m a
readInt s = case reads s of
              [(n,"")] -> return $ fromInteger n
              _        -> F.failure $ XmlException "Invalid Integer"

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
xmlCursorConsumer parse metadataRef _status _headers source
    = do doc <- source $$ XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         let Response metadata x = parse cursor
         liftIO $ tellMetadataRef metadataRef metadata
         case x of
           Failure err -> liftIO $ C.monadThrow err
           Success v   -> return v
