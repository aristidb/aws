module Aws.Sqs.Core where

import           Aws.Core
import           Aws.S3.Core                    (LocationConstraint, locationUsClassic, locationUsWest, locationApSouthEast, locationApNorthEast, locationEu)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attempt                   (Attempt(..))
import           Data.Conduit                   (($$+-))
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time
import           Data.Typeable
import           System.Locale
import           Text.XML.Cursor                (($/))
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Control.Exception              as C
import qualified Control.Failure                as F
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.Conduit                   as C
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Encoding             as TE
import qualified Network.HTTP.Types             as HTTP
import qualified Text.XML                       as XML
import qualified Text.XML.Cursor                as Cu

type ErrorCode = T.Text

data SqsError
    = SqsError {
        sqsStatusCode :: HTTP.Status
      , sqsErrorCode :: ErrorCode
      , sqsErrorType :: T.Text
      , sqsErrorMessage :: T.Text
      , sqsErrorDetail :: Maybe T.Text
      , sqsErrorMetadata :: Maybe SqsMetadata
      }
    | SqsXmlError { 
        sqsXmlErrorMessage :: T.Text
      , sqsXmlErrorMetadata :: Maybe SqsMetadata
      }
    deriving (Show, Typeable)

instance C.Exception SqsError

data SqsMetadata
    = SqsMetadata {
        sqsMAmzId2 :: Maybe T.Text
      , sqsMRequestId :: Maybe T.Text
      }
    deriving (Show)

instance Monoid SqsMetadata where
    mempty = SqsMetadata Nothing Nothing
    SqsMetadata a1 r1 `mappend` SqsMetadata a2 r2 = SqsMetadata (a1 `mplus` a2) (r1 `mplus` r2)

data SqsAuthorization 
    = SqsAuthorizationHeader 
    | SqsAuthorizationQuery
    deriving (Show)

data Endpoint
    = Endpoint {
        endpointHost :: B.ByteString
      , endpointDefaultLocationConstraint :: LocationConstraint
      , endpointAllowedLocationConstraints :: [LocationConstraint]
      }
    deriving (Show)

data SqsConfiguration qt
    = SqsConfiguration {
        sqsProtocol :: Protocol
      , sqsEndpoint :: Endpoint
      , sqsPort :: Int
      , sqsUseUri :: Bool
      , sqsDefaultExpiry :: NominalDiffTime
      }
    deriving (Show)

instance DefaultServiceConfiguration (SqsConfiguration NormalQuery) where
    defServiceConfig = sqs HTTPS sqsEndpointUsClassic False
    debugServiceConfig = sqs HTTP sqsEndpointUsClassic False

instance DefaultServiceConfiguration (SqsConfiguration UriOnlyQuery) where
    defServiceConfig = sqs HTTPS sqsEndpointUsClassic True
    debugServiceConfig = sqs HTTP sqsEndpointUsClassic True
  
sqsEndpointUsClassic :: Endpoint
sqsEndpointUsClassic 
    = Endpoint { 
        endpointHost = "queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationUsClassic
      , endpointAllowedLocationConstraints = [locationUsClassic
                                             , locationUsWest
                                             , locationEu
                                             , locationApSouthEast
                                             , locationApNorthEast]
      }

sqsEndpointUsWest :: Endpoint
sqsEndpointUsWest
    = Endpoint {
        endpointHost = "us-west-1.queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationUsWest
      , endpointAllowedLocationConstraints = [locationUsWest]
      }

sqsEndpointEu :: Endpoint
sqsEndpointEu
    = Endpoint {
        endpointHost = "eu-west-1.queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationEu
      , endpointAllowedLocationConstraints = [locationEu]
      }

sqsEndpointApSouthEast :: Endpoint
sqsEndpointApSouthEast
    = Endpoint {
        endpointHost = "ap-southeast-1.queue.amazonaws.com"
      , endpointDefaultLocationConstraint = locationApSouthEast
      , endpointAllowedLocationConstraints = [locationApSouthEast]
      }

sqsEndpointApNorthEast :: Endpoint
sqsEndpointApNorthEast
    = Endpoint {
        endpointHost = "sqs.ap-northeast-1.amazonaws.com"
      , endpointDefaultLocationConstraint = locationApNorthEast
      , endpointAllowedLocationConstraints = [locationApNorthEast]
      }

sqs :: Protocol -> Endpoint -> Bool -> SqsConfiguration qt
sqs protocol endpoint uri 
    = SqsConfiguration { 
        sqsProtocol = protocol
      , sqsEndpoint = endpoint
      , sqsPort = defaultPort protocol
      , sqsUseUri = uri
      , sqsDefaultExpiry = 15*60
      }

data SqsQuery = SqsQuery{
  sqsQueueName :: Maybe QueueName,
  sqsQuery :: HTTP.Query
}

sqsSignQuery :: SqsQuery -> SqsConfiguration qt -> SignatureData -> SignedQuery
sqsSignQuery SqsQuery{..} SqsConfiguration{..} SignatureData{..}
    = SignedQuery {
        sqMethod = method
      , sqProtocol = sqsProtocol
      , sqHost = endpointHost sqsEndpoint
      , sqPort = sqsPort
      , sqPath = path
      , sqQuery = signedQuery
      , sqDate = Just signatureTime
      , sqAuthorization = Nothing 
      , sqBody = Nothing
      , sqStringToSign = stringToSign
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      , sqAmzHeaders = []
      , sqOtherHeaders = []
      }
    where
      method = PostQuery
      path = case sqsQueueName of
                Just x -> TE.encodeUtf8 $ printQueueName x
                Nothing -> "/"
      expandedQuery = sortBy (comparing fst) 
                       ( sqsQuery ++ [ ("AWSAccessKeyId", Just(accessKeyID signatureCredentials)), 
                       ("Expires", Just(BC.pack expiresString)), 
                       ("SignatureMethod", Just("HmacSHA256")), ("SignatureVersion",Just("2")), ("Version",Just("2009-02-01"))

                       ])
      
      expires = AbsoluteExpires $ sqsDefaultExpiry `addUTCTime` signatureTime

      expiresString = formatTime defaultTimeLocale "%FT%TZ" (fromAbsoluteTimeInfo expires)

      sig = signature signatureCredentials HmacSHA256 stringToSign
      stringToSign = Blaze.toByteString . mconcat . intersperse (Blaze8.fromChar '\n') . concat  $
                       [[Blaze.copyByteString $ httpMethod method]
                       , [Blaze.copyByteString $ endpointHost sqsEndpoint]
                       , [Blaze.copyByteString path]
                       , [Blaze.copyByteString $ HTTP.renderQuery False expandedQuery ]]

      signedQuery = expandedQuery ++ (HTTP.simpleQueryToQuery $ makeAuthQuery)

      makeAuthQuery = [("Signature", sig)]

sqsResponseConsumer :: HTTPResponseConsumer a
                    -> IORef SqsMetadata
                    -> HTTPResponseConsumer a
sqsResponseConsumer inner metadata status headers source = do
      let headerString = fmap T.decodeUtf8 . flip lookup headers
      let amzId2 = headerString "x-amz-id-2"
      let requestId = headerString "x-amz-request-id"

      let m = SqsMetadata { sqsMAmzId2 = amzId2, sqsMRequestId = requestId }
      liftIO $ tellMetadataRef metadata m

      if status >= HTTP.status400
        then sqsErrorResponseConsumer status headers source
        else inner status headers source

sqsXmlResponseConsumer :: (Cu.Cursor -> Response SqsMetadata a)
                       -> IORef SqsMetadata
                       -> HTTPResponseConsumer a
sqsXmlResponseConsumer parse metadataRef = sqsResponseConsumer (xmlCursorConsumer parse metadataRef) metadataRef

sqsErrorResponseConsumer :: HTTPResponseConsumer a
sqsErrorResponseConsumer status _headers source
    = do doc <- source $$+- XML.sinkDoc XML.def
         let cursor = Cu.fromDocument doc
         liftIO $ case parseError cursor of
           Success err -> C.monadThrow err
           Failure otherErr -> C.monadThrow otherErr
    where
      parseError :: Cu.Cursor -> Attempt SqsError
      parseError root = do cursor <- force "Missing Error" $ root $/ Cu.laxElement "Error"
                           code <- force "Missing error Code" $ cursor $/ elContent "Code"
                           message <- force "Missing error Message" $ cursor $/ elContent "Message"
                           errorType <- force "Missing error Type" $ cursor $/ elContent "Type"
                           let detail = listToMaybe $ cursor $/ elContent "Detail"

                           return SqsError {
                                        sqsStatusCode = status
                                      , sqsErrorCode = code
                                      , sqsErrorMessage = message
                                      , sqsErrorType = errorType
                                      , sqsErrorDetail = detail
                                      , sqsErrorMetadata = Nothing
                                      }

data QueueName = QueueName{
  qName :: T.Text,
  qAccountNumber :: T.Text
} deriving(Show)

printQueueName :: QueueName -> T.Text
printQueueName queue = T.concat ["/", (qAccountNumber queue), "/", (qName queue), "/"]

data QueueAttribute
    = QueueAll
    | ApproximateNumberOfMessages
    | ApproximateNumberOfMessagesNotVisible
    | VisibilityTimeout
    | CreatedTimestamp
    | LastModifiedTimestamp
    | Policy
    | MaximumMessageSize
    | MessageRetentionPeriod
    | QueueArn
    deriving(Show, Enum, Eq)

data MessageAttribute
    = MessageAll
    | SenderId
    | SentTimestamp
    | ApproximateReceiveCount
    | ApproximateFirstReceiveTimestamp
    deriving(Show,Eq,Enum)

data SqsPermission
    = PermissionAll
    | PermissionSendMessage
    | PermissionReceiveMessage
    | PermissionDeleteMessage
    | PermissionChangeMessageVisibility
    | PermissionGetQueueAttributes
    deriving (Show, Enum, Eq)

parseQueueAttribute :: F.Failure XmlException m  => T.Text -> m QueueAttribute
parseQueueAttribute "ApproximateNumberOfMessages" = return ApproximateNumberOfMessages 
parseQueueAttribute "ApproximateNumberOfMessagesNotVisible" = return ApproximateNumberOfMessagesNotVisible
parseQueueAttribute "VisibilityTimeout" = return VisibilityTimeout
parseQueueAttribute "CreatedTimestamp" = return CreatedTimestamp
parseQueueAttribute "LastModifiedTimestamp" = return LastModifiedTimestamp
parseQueueAttribute "Policy" = return Policy
parseQueueAttribute "MaximumMessageSize" = return MaximumMessageSize
parseQueueAttribute "MessageRetentionPeriod" = return MessageRetentionPeriod
parseQueueAttribute "QueueArn" = return QueueArn
parseQueueAttribute x = F.failure $ XmlException ( "Invalid Attribute Name. " ++ show x)

printQueueAttribute :: QueueAttribute -> T.Text
printQueueAttribute QueueAll = "All"
printQueueAttribute ApproximateNumberOfMessages = "ApproximateNumberOfMessages"
printQueueAttribute ApproximateNumberOfMessagesNotVisible = "ApproximateNumberOfMessagesNotVisible"
printQueueAttribute VisibilityTimeout = "VisibilityTimeout"
printQueueAttribute CreatedTimestamp = "CreatedTimestamp"
printQueueAttribute LastModifiedTimestamp = "LastModifiedTimestamp"
printQueueAttribute Policy = "Policy"
printQueueAttribute MaximumMessageSize = "MaximumMessageSize"
printQueueAttribute MessageRetentionPeriod = "MessageRetentionPeriod"
printQueueAttribute QueueArn = "QueueArn"

parseMessageAttribute :: F.Failure XmlException m  =>  T.Text -> m MessageAttribute
parseMessageAttribute "SenderId" = return SenderId
parseMessageAttribute "SentTimestamp" = return SentTimestamp
parseMessageAttribute "ApproximateReceiveCount" = return ApproximateReceiveCount
parseMessageAttribute "ApproximateFirstReceiveTimestamp" = return ApproximateFirstReceiveTimestamp
parseMessageAttribute x = F.failure $ XmlException ( "Invalid Attribute Name. " ++ show x)

printMessageAttribute :: MessageAttribute -> T.Text
printMessageAttribute MessageAll = "All"
printMessageAttribute SenderId = "SenderId"
printMessageAttribute SentTimestamp = "SentTimestamp"
printMessageAttribute ApproximateReceiveCount = "ApproximateReceiveCount"
printMessageAttribute ApproximateFirstReceiveTimestamp = "ApproximateFirstReceiveTimestamp"

printPermission :: SqsPermission -> T.Text
printPermission PermissionAll = "*"
printPermission PermissionSendMessage = "SendMessage"
printPermission PermissionReceiveMessage = "ReceiveMessage"
printPermission PermissionDeleteMessage = "DeleteMessage"
printPermission PermissionChangeMessageVisibility = "ChangeMessageVisibility"
printPermission PermissionGetQueueAttributes = "GetQueueAttributes"

newtype ReceiptHandle = ReceiptHandle T.Text deriving(Show,Eq)
newtype MessageId = MessageId T.Text deriving(Show,Eq)

printReceiptHandle :: ReceiptHandle -> T.Text
printReceiptHandle (ReceiptHandle handle) = handle 
