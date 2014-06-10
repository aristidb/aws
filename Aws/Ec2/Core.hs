module Aws.Ec2.Core
    ( -- * Configuration
      Ec2Configuration(..)
    , ec2EndpointDefault
    , ec2

      -- * Errors
    , Ec2Error(..)

      -- * Metadata
    , Ec2Metadata(..)

      -- * Queries
    , ec2SignQuery
    , ec2Action
    ) where

import           Control.Arrow (second)
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString ()
import           Data.List (intersperse, sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text ()
import qualified Data.Text.Encoding as T
import           Data.Typeable

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

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
    = Ec2Error
    deriving (Show, Typeable)

-- -------------------------------------------------------------------------- --
-- Response metadata

-- | Metadata about an EC2 API request.
data Ec2Metadata
    = Ec2Metadata {
        requestId :: Maybe Text
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
                        , ("SignatureMethod" , amzHash HmacSHA256)
                        , ("SignatureVersion", "2")
                        , ("Version"         , "2010-05-08")
                        , timestampHeader
                        ]

-- -------------------------------------------------------------------------- --
-- Response
