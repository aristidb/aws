module Aws.DynamoDb.Core where

import           Aws.Core
import qualified Control.Exception              as C
import           Control.Monad.Trans.Resource   (throwM)
import           Crypto.Hash
import           Data.Byteable
import qualified Data.Aeson                     as A
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base16         as Base16
import           Data.Conduit
import qualified Data.Conduit.Attoparsec        as Atto
import           Data.Monoid
import           Data.Typeable
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP

type ErrorCode = String

data DyError
    = DyError {
        dyStatusCode :: HTTP.Status
      , dyErrorCode :: ErrorCode
      , dyErrorMessage :: String
      }
    deriving (Show, Typeable)

instance C.Exception DyError

data DyMetadata = DyMetadata
    deriving (Show, Typeable)

instance Loggable DyMetadata where
    toLogText DyMetadata = "DynamoDB"

instance Monoid DyMetadata where
    mempty = DyMetadata
    DyMetadata `mappend` DyMetadata = DyMetadata

data DyConfiguration qt
    = DyConfiguration {
        dyProtocol :: Protocol
      , dyHost :: B.ByteString
      , dyPort :: Int
      , dyRegion :: B.ByteString
      }
    deriving (Show)

instance DefaultServiceConfiguration (DyConfiguration NormalQuery) where
  defServiceConfig = dyHttp dyUsEast
  debugServiceConfig = dyLocal

dyUsEast :: (B.ByteString, B.ByteString)
dyUsEast = ("us-east-1", "dynamodb.us-east-1.amazonaws.com")

dyHttp :: (B.ByteString, B.ByteString) -> DyConfiguration qt
dyHttp (region, endpoint) = DyConfiguration HTTP endpoint (defaultPort HTTP) region

dyHttps :: (B.ByteString, B.ByteString) -> DyConfiguration qt
dyHttps (region, endpoint) = DyConfiguration HTTPS endpoint (defaultPort HTTPS) region

dyLocal :: DyConfiguration qt
dyLocal = DyConfiguration HTTP "localhost" 8000 "local"

dyApiVersion :: B.ByteString
dyApiVersion = "DynamoDB_20120810."

dySignQuery :: A.ToJSON a => B.ByteString -> a -> DyConfiguration qt -> SignatureData -> SignedQuery
dySignQuery target body di sd
    = SignedQuery {
        sqMethod = Post
      , sqProtocol = dyProtocol di
      , sqHost = dyHost di
      , sqPort = dyPort di
      , sqPath = "/"
      , sqQuery = []
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Just auth
      , sqContentType = Just "application/x-amz-json-1.0"
      , sqContentMd5 = Nothing
      , sqAmzHeaders = [ ("X-Amz-Target", dyApiVersion <> target)
                       , ("X-Amz-Date", sigTime)
                       ]
      , sqOtherHeaders = []
      , sqBody = Just $ HTTP.RequestBodyLBS bodyLBS
      , sqStringToSign = canonicalRequest
      }
    where
        sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sd

        bodyLBS = A.encode body
        bodyHash = Base16.encode $ toBytes (hashlazy bodyLBS :: Digest SHA256)

        canonicalRequest = B.concat [ "POST\n"
                                    , "/\n"
                                    , "\n" -- query string
                                    , "content-type:application/x-amz-json-1.0\n"
                                    , "host:"
                                    , dyHost di
                                    , "\n"
                                    , "x-amz-date:"
                                    , sigTime
                                    , "\n"
                                    , "x-amz-target:"
                                    , dyApiVersion
                                    , target
                                    , "\n"
                                    , "\n" -- end headers
                                    , "content-type;host;x-amz-date;x-amz-target\n"
                                    , bodyHash
                                    ]

        auth = authorizationV4 sd HmacSHA256 (dyRegion di) "dynamodb"
                               "content-type;host;x-amz-date;x-amz-target"
                               canonicalRequest

dyResponseConsumer :: A.FromJSON a
                   => HTTPResponseConsumer a
dyResponseConsumer resp = do
    val <- HTTP.responseBody resp $$+- Atto.sinkParser A.json'
    case HTTP.responseStatus resp of
        (HTTP.Status{HTTP.statusCode=200}) -> do
            case A.fromJSON val of
                A.Success a -> return a
                A.Error err -> throwM $ DyError (HTTP.responseStatus resp) "" err
        _ -> throwM $ DyError (HTTP.responseStatus resp) "" (show val)
