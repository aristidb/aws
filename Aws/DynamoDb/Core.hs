module Aws.DynamoDb.Core where

import           Aws.Core
import qualified Control.Exception              as C
import           Crypto.HMAC (hmac', MacKey(..))
import           Crypto.Hash.CryptoAPI (SHA256, hash, hash')
import qualified Data.Aeson                     as A
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base16         as Base16
import qualified Data.ByteString.Lazy           as BL
import           Data.Conduit
import qualified Data.Conduit.Attoparsec        as Atto
import           Data.Monoid
import           Data.Typeable
import qualified Data.Serialize                 as Serialize
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

-- | Create a signature for Signature Version 4
signatureV4 :: DyConfiguration qt -> Credentials -> B.ByteString -> B.ByteString -> B.ByteString
signatureV4 di cr sigDate input = Base16.encode sig
    where
      hmac256 :: B.ByteString -> B.ByteString -> B.ByteString
      hmac256 k i = Serialize.encode (hmac' (MacKey k) i :: SHA256) 

      kDate = hmac256 ("AWS4" <> secretAccessKey cr) sigDate
      kRegion = hmac256 kDate $ dyRegion di
      kService = hmac256 kRegion "dynamodb"
      kSigning = hmac256 kService "aws4_request"

      sig = hmac256 kSigning input

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
      , sqAuthorization = Just authorization
      , sqContentType = Just "application/x-amz-json-1.0"
      , sqContentMd5 = Nothing
      , sqAmzHeaders = [ ("X-Amz-Target", dyApiVersion <> target)
                       , ("X-Amz-Date", sigTime)
                       ]
      , sqOtherHeaders = []
      , sqBody = Just $ HTTP.RequestBodyLBS bodyLBS
      , sqStringToSign = stringToSign
      }
    where
        sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sd
        sigDate = fmtTime "%Y%m%d" $ signatureTime sd

        hash256 :: BL.ByteString -> SHA256
        hash256 = hash
        hash256' :: B.ByteString -> SHA256
        hash256' = hash'

        bodyLBS = A.encode body
        bodyHash = Base16.encode $ Serialize.encode $ hash256 bodyLBS

        canonicalRequest = B.concat [ "POST\n"
                                    , "/\n"
                                    , "\n" -- query string
                                    , "content-type:application/x-amz-json-1.0\n"
                                    , "host:"
                                    , dyHost di
                                    , "\n"
                                    , "x-amz-date:"
                                    , sigDate
                                    , "\n"
                                    , "x-amz-target:"
                                    , dyApiVersion
                                    , target
                                    , "\n"
                                    , "\n" -- end headers
                                    , "content-type;host;x-amz-date;x-amz-target\n"
                                    , bodyHash
                                    ]

        canonicalRequestHash = Base16.encode $ Serialize.encode $ hash256' canonicalRequest

        stringToSign = B.concat [ "AWS4-HMAC-SHA256\n"
                                , sigTime
                                , "\n"
                                , sigDate
                                , "/"
                                , dyRegion di
                                , "/dynamodb/aws4_request\n"
                                , canonicalRequestHash
                                ]

        sig = signatureV4 di (signatureCredentials sd) sigDate stringToSign

        authorization = B.concat [ "AWS4-HMAC-SHA256 Credential="
                                 , accessKeyID (signatureCredentials sd)
                                 , "/"
                                 , sigDate
                                 , "/"
                                 , dyRegion di
                                 , "/dynamodb/aws4_request,"
                                 , "SignedHeaders=content-type;host;x-amz-date;x-amz-target,"
                                 , "Signature="
                                 , sig
                                 ]

dyResponseConsumer :: A.FromJSON a
                   => HTTPResponseConsumer a
dyResponseConsumer resp = do
    val <- HTTP.responseBody resp $$+- Atto.sinkParser A.json'
    case A.fromJSON val of
        A.Success a -> return a
        A.Error err -> monadThrow $ DyError (HTTP.responseStatus resp) "" err
