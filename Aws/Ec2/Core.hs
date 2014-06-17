module Aws.Ec2.Core
    ( -- * Configuration
      Ec2Configuration(..)
    , ec2EndpointDefault
    , ec2

      -- * Errors
    , Ec2Error(..)

      -- * Metadata
    , Ec2Metadata(..)

      -- * Data
    , Ec2Region(..)
    , parseRegion

      -- * Queries
    , ec2SignQuery
    , ec2Action

      -- * Request/Response
    , ec2ResponseConsumer
    ) where

import           Control.Arrow (second)
import           Control.Exception              (Exception)
import           Control.Monad
import           Control.Monad.Trans.Resource   (MonadThrow, throwM)
import           Data.ByteString (ByteString)
import qualified Data.ByteString ()
import           Data.IORef
import           Data.List (intersperse, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import           Text.XML.Cursor                (($//))
import qualified Text.XML.Cursor                as Cu

import           Aws.Core

-- -------------------------------------------------------------------------- --
-- Configuration

-- | Configuration for EC2 API transactions.
data Ec2Configuration qt
    = Ec2Configuration {
        ec2Endpoint :: ByteString
      , ec2Port :: Int
      , ec2Protocol :: Protocol
      , ec2HttpMethod :: Method
      }
      deriving (Show)

instance DefaultServiceConfiguration (Ec2Configuration NormalQuery) where
    defServiceConfig   = ec2 PostQuery HTTPS ec2EndpointDefault
    debugServiceConfig = ec2 PostQuery HTTP  ec2EndpointDefault

instance DefaultServiceConfiguration (Ec2Configuration UriOnlyQuery) where
    defServiceConfig   = ec2 Get HTTPS ec2EndpointDefault
    debugServiceConfig = ec2 Get HTTP  ec2EndpointDefault

-- | Construct an EC2 API configuration.
ec2 :: Method -> Protocol -> ByteString -> Ec2Configuration qt
ec2 method protocol endpoint
    = Ec2Configuration {
        ec2Endpoint   = endpoint
      , ec2Protocol   = protocol
      , ec2Port       = defaultPort protocol
      , ec2HttpMethod = method
      }

-- | Default EC2 endpoint to communicate with.
ec2EndpointDefault :: ByteString
ec2EndpointDefault = "ec2.amazonaws.com"


-- -------------------------------------------------------------------------- --
-- Errors

-- | Errors returned by the EC2 API.
data Ec2Error
    = Ec2Error {
        ec2StatusCode   :: HTTP.Status -- ^ HTTP status of the request.
      , ec2ErrorCode    :: Text -- ^ AWS API error code.
      , ec2ErrorMessage :: Text -- ^ AWS API error message.
      }
    deriving (Show, Typeable)

instance Exception Ec2Error

-- -------------------------------------------------------------------------- --
-- Response metadata

-- | Metadata about an EC2 API request.
data Ec2Metadata
    = Ec2Metadata {
        requestId :: Maybe Text -- ^ The identifier assigned by AWS to the request.
      }
    deriving (Show, Typeable)

instance Loggable Ec2Metadata where
    toLogText (Ec2Metadata rid) = "Ec2: request ID=" `mappend`
                                  fromMaybe "<none>" rid

instance Monoid Ec2Metadata where
    mempty = Ec2Metadata Nothing
    Ec2Metadata r1 `mappend` Ec2Metadata r2 = Ec2Metadata (r1 `mplus` r2)

-- -------------------------------------------------------------------------- --
-- Request signing

-- | Sign an "action" request against the EC2 API.
ec2Action :: Text -- ^ Action name.
          -> [(ByteString, Text)] -- ^ Query parameters.
          -> Ec2Configuration qt
          -> SignatureData
          -> SignedQuery
ec2Action action = ec2SignQuery
                   . map (second T.encodeUtf8)
                   . (:) ("Action", action)

-- | Sign a query for destined the EC2 API.
--
-- This is used to implement 'signQuery' for EC2 commands.
ec2SignQuery :: [(ByteString, ByteString)]
             -> Ec2Configuration qt
             -> SignatureData
             -> SignedQuery
ec2SignQuery q Ec2Configuration{..} SignatureData{..}
    = SignedQuery {
        sqMethod        = ec2HttpMethod
      , sqProtocol      = ec2Protocol
      , sqHost          = ec2Endpoint
      , sqPort          = ec2Port
      , sqPath          = "/"
      , sqQuery         = signedQuery
      , sqDate          = Just signatureTime
      , sqAuthorization = Nothing
      , sqContentType   = Nothing
      , sqContentMd5    = Nothing
      , sqAmzHeaders    = []
      , sqOtherHeaders  = []
      , sqBody          = Nothing
      , sqStringToSign  = stringToSign
      }
  where
    sig             = signature signatureCredentials HmacSHA256 stringToSign
    signedQuery     = ("Signature", Just sig):expandedQuery
    accessKey       = accessKeyID signatureCredentials
    timestampHeader = case signatureTimeInfo of
      AbsoluteTimestamp time -> ("Timestamp", fmtAmzTime time)
      AbsoluteExpires   time -> ("Expires"  , fmtAmzTime time)
    newline         = Blaze8.fromChar '\n'
    stringToSign    = Blaze.toByteString . mconcat . intersperse newline $
                        map Blaze.copyByteString
                           [httpMethod ec2HttpMethod, ec2Endpoint, "/"]
                        ++ [HTTP.renderQueryBuilder False expandedQuery]
    expandedQuery   = HTTP.toQuery . sort $ (q ++) [
                          ("AWSAccessKeyId"  , accessKey)
                        , timestampHeader
                        , ("SignatureVersion", "2")
                        , ("SignatureMethod" , amzHash HmacSHA256)
                        , ("Version"         , "2014-05-01")
                        ]

-- -------------------------------------------------------------------------- --
-- Response

ec2ResponseConsumer :: (Cu.Cursor -> Response Ec2Metadata a)
                    -> IORef Ec2Metadata
                    -> HTTPResponseConsumer a
ec2ResponseConsumer inner md resp = xmlCursorConsumer parse md resp
  where
    parse cursor = do
      let rid = listToMaybe $ cursor $// elContent "requestID"
      tellMetadata $ Ec2Metadata rid
      case cursor $// Cu.laxElement "Error" of
        []      -> inner cursor
        (err:_) -> fromError err
    fromError cursor = do
      errCode <- force "Missing Error Code"    $ cursor $// elContent "Code"
      errMsg  <- force "Missing Error Message" $ cursor $// elContent "Message"
      throwM $ Ec2Error (HTTP.responseStatus resp) errCode errMsg


-- -------------------------------------------------------------------------- --
-- Data Model

-- | Details of an EC2 region.
data Ec2Region
    = Ec2Region {
        regionName :: Text -- ^ Canonical name of the region.
      , regionEndpoint :: Text -- ^ API endpoint for the region.
      }
    deriving (Show, Ord, Eq, Typeable)

-- | Parse an 'Ec2Region' from an API response.
parseRegion :: MonadThrow m => Cu.Cursor -> m Ec2Region
parseRegion cursor = do
    regionName <- attr "regionName"
    regionEndpoint <- attr "regionEndpoint"
    return Ec2Region{..}
  where
    attr n = force ("Missing " ++ T.unpack n) $
             cursor $// elContent n
