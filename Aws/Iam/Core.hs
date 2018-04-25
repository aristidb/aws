{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Aws.Iam.Core
    ( iamSignQuery
    , iamResponseConsumer
    , IamMetadata(..)
    , IamConfiguration(..)
    , IamError(..)

    , parseDateTime

    , AccessKeyStatus(..)
    , User(..)
    , parseUser
    , MfaDevice(..)
    , parseMfaDevice
    ) where

import           Aws.Core
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import           Control.Exception              (Exception)
import           Control.Monad
import           Control.Monad.Trans.Resource   (MonadThrow, throwM)
import           Data.ByteString                (ByteString)
import           Data.IORef
import           Data.List                      (intersperse, sort)
import           Data.Maybe
import           Data.Monoid                    ()
import qualified Data.Semigroup                 as Sem
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Time
import           Data.Typeable
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP
#if !MIN_VERSION_time(1,5,0)
import           System.Locale
#endif
import           Text.XML.Cursor                (($//))
import qualified Text.XML.Cursor                as Cu

data IamError
    = IamError {
        iamStatusCode   :: HTTP.Status
      , iamErrorCode    :: Text
      , iamErrorMessage :: Text
      }
    deriving (Show, Typeable)

instance Exception IamError

data IamMetadata
    = IamMetadata {
        requestId :: Maybe Text
      }
    deriving (Show, Typeable)

instance Loggable IamMetadata where
    toLogText (IamMetadata r) = "IAM: request ID=" Sem.<> fromMaybe "<none>" r

instance Sem.Semigroup IamMetadata where
    IamMetadata r1 <> IamMetadata r2 = IamMetadata (r1 `mplus` r2)

instance Monoid IamMetadata where
    mempty = IamMetadata Nothing
    mappend = (Sem.<>)

data IamConfiguration qt
    = IamConfiguration {
        iamEndpoint   :: ByteString
      , iamPort       :: Int
      , iamProtocol   :: Protocol
      , iamHttpMethod :: Method
      }
    deriving (Show)

instance DefaultServiceConfiguration (IamConfiguration NormalQuery) where
    defServiceConfig   = iam PostQuery HTTPS iamEndpointDefault
    debugServiceConfig = iam PostQuery HTTP  iamEndpointDefault

instance DefaultServiceConfiguration (IamConfiguration UriOnlyQuery) where
    defServiceConfig   = iam Get HTTPS iamEndpointDefault
    debugServiceConfig = iam Get HTTP  iamEndpointDefault

-- | The default IAM endpoint.
iamEndpointDefault :: ByteString
iamEndpointDefault = "iam.amazonaws.com"

-- | Constructs an IamConfiguration with the specified parameters.
iam :: Method -> Protocol -> ByteString -> IamConfiguration qt
iam method protocol endpoint
    = IamConfiguration {
        iamEndpoint   = endpoint
      , iamProtocol   = protocol
      , iamPort       = defaultPort protocol
      , iamHttpMethod = method
      }

-- | Constructs a 'SignedQuery' with the specified request parameters.
iamSignQuery
    :: [(ByteString, ByteString)]
    -- ^ Pairs of parameter names and values that will be passed as part of
    -- the request data.
    -> IamConfiguration qt
    -> SignatureData
    -> SignedQuery
iamSignQuery q IamConfiguration{..} SignatureData{..}
    = SignedQuery {
        sqMethod        = iamHttpMethod
      , sqProtocol      = iamProtocol
      , sqHost          = iamEndpoint
      , sqPort          = iamPort
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
      timestampHeader =
          case signatureTimeInfo of
            AbsoluteTimestamp time -> ("Timestamp", Just $ fmtAmzTime time)
            AbsoluteExpires   time -> ("Expires"  , Just $ fmtAmzTime time)
      newline         = Blaze8.fromChar '\n'
      stringToSign    = Blaze.toByteString . mconcat . intersperse newline $
                            map Blaze.copyByteString
                                [httpMethod iamHttpMethod, iamEndpoint, "/"]
                            ++  [HTTP.renderQueryBuilder False expandedQuery]
      expandedQuery   = HTTP.toQuery . sort $ (map (\(a,b) -> (a, Just b)) q ++) [
                            ("AWSAccessKeyId"  , Just accessKey)
                          , ("SignatureMethod" , Just $ amzHash HmacSHA256)
                          , ("SignatureVersion", Just "2")
                          , ("Version"         , Just "2010-05-08")
                          , timestampHeader] ++
                          maybe [] (\tok -> [ ("SecurityToken", Just tok)]) (iamToken signatureCredentials)

-- | Reads the metadata from an IAM response and delegates parsing the rest of
-- the data from the response to the given function.
iamResponseConsumer :: (Cu.Cursor -> Response IamMetadata a)
                    -> IORef IamMetadata
                    -> HTTPResponseConsumer a
iamResponseConsumer inner md resp = xmlCursorConsumer parse md resp
  where
    parse cursor = do
      let rid = listToMaybe $ cursor $// elContent "RequestID"
      tellMetadata $ IamMetadata rid
      case cursor $// Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err
    fromError cursor = do
      errCode <- force "Missing Error Code"    $ cursor $// elContent "Code"
      errMsg  <- force "Missing Error Message" $ cursor $// elContent "Message"
      throwM $ IamError (HTTP.responseStatus resp) errCode errMsg

-- | Parses IAM @DateTime@ data type.
parseDateTime :: MonadThrow m => String -> m UTCTime
parseDateTime x
    = case parseTimeM True defaultTimeLocale iso8601UtcDate x of
        Nothing -> throwM $ XmlException $ "Invalid DateTime: " ++ x
        Just dt -> return dt

-- | The IAM @User@ data type.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_User.html>
data User
    = User {
        userArn        :: Text
      -- ^ ARN used to refer to this user.
      , userCreateDate :: UTCTime
      -- ^ Date and time at which the user was created.
      , userPath       :: Text
      -- ^ Path under which the user was created.
      , userUserId     :: Text
      -- ^ Unique identifier used to refer to this user. 
      , userUserName   :: Text
      -- ^ Name of the user.
      }
    deriving (Eq, Ord, Show, Typeable)

-- | Parses the IAM @User@ data type.
parseUser :: MonadThrow m => Cu.Cursor -> m User
parseUser cursor = do
    userArn        <- attr "Arn"
    userCreateDate <- attr "CreateDate" >>= parseDateTime . Text.unpack
    userPath       <- attr "Path"
    userUserId     <- attr "UserId"
    userUserName   <- attr "UserName"
    return User{..}
  where
    attr name = force ("Missing " ++ Text.unpack name) $
                cursor $// elContent name


data AccessKeyStatus = AccessKeyActive | AccessKeyInactive
    deriving (Eq, Ord, Show, Typeable)

-- | The IAM @MFADevice@ data type.
--
-- <https://docs.aws.amazon.com/IAM/latest/APIReference/API_MFADevice.html>
data MfaDevice = MfaDevice
                 { mfaEnableDate   :: UTCTime
                   -- ^ The date when the MFA device was enabled for
                   -- the user.
                 , mfaSerialNumber :: Text
                   -- ^ The serial number that uniquely identifies the
                   -- MFA device. For virtual MFA devices, the serial
                   -- number is the device ARN.
                 , mfaUserName     :: Text
                   -- ^ The user with whom the MFA device is
                   -- associated. Minimum length of 1. Maximum length
                   -- of 64.
                 } deriving (Eq, Ord, Show, Typeable)

-- | Parses the IAM @MFADevice@ data type.
parseMfaDevice :: MonadThrow m => Cu.Cursor -> m MfaDevice
parseMfaDevice cursor = do
  mfaEnableDate   <- attr "EnableDate" >>= parseDateTime . Text.unpack
  mfaSerialNumber <- attr "SerialNumber"
  mfaUserName     <- attr "UserName"
  return MfaDevice{..}
 where attr name = force ("Missing " ++ Text.unpack name) $
               cursor $// elContent name
