module Aws.SimpleDb.Core where

import           Aws.Core
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Control.Exception              as C
import           Control.Monad
import           Control.Monad.Trans.Resource   (MonadThrow, throwM)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base64         as Base64
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup                 as Sem
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Typeable
import           Prelude
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP
import           Text.XML.Cursor                (($|), ($/), ($//), (&|))
import qualified Text.XML.Cursor                as Cu

type ErrorCode = String

data SdbError
    = SdbError {
        sdbStatusCode :: HTTP.Status
      , sdbErrorCode :: ErrorCode
      , sdbErrorMessage :: String
      }
    deriving (Show, Typeable)

instance C.Exception SdbError

data SdbMetadata
    = SdbMetadata {
        requestId :: Maybe T.Text
      , boxUsage :: Maybe T.Text
      }
    deriving (Show, Typeable)

instance Loggable SdbMetadata where
    toLogText (SdbMetadata rid bu) = "SimpleDB: request ID=" `mappend`
                                     fromMaybe "<none>" rid `mappend`
                                     ", box usage=" `mappend`
                                     fromMaybe "<not available>" bu

instance Sem.Semigroup SdbMetadata where
    SdbMetadata r1 b1 <> SdbMetadata r2 b2 = SdbMetadata (r1 `mplus` r2) (b1 `mplus` b2)

instance Monoid SdbMetadata where
    mempty = SdbMetadata Nothing Nothing
    mappend = (Sem.<>)

data SdbConfiguration qt
    = SdbConfiguration {
        sdbiProtocol :: Protocol
      , sdbiHttpMethod :: Method
      , sdbiHost :: B.ByteString
      , sdbiPort :: Int
      }
    deriving (Show)

instance DefaultServiceConfiguration (SdbConfiguration NormalQuery) where
  defServiceConfig = sdbHttpsPost sdbUsEast
  debugServiceConfig = sdbHttpPost sdbUsEast

instance DefaultServiceConfiguration (SdbConfiguration UriOnlyQuery) where
  defServiceConfig = sdbHttpsGet sdbUsEast
  debugServiceConfig = sdbHttpGet sdbUsEast

sdbUsEast :: B.ByteString
sdbUsEast = "sdb.amazonaws.com"

sdbUsWest :: B.ByteString
sdbUsWest = "sdb.us-west-1.amazonaws.com"

sdbEuWest :: B.ByteString
sdbEuWest = "sdb.eu-west-1.amazonaws.com"

sdbApSoutheast :: B.ByteString
sdbApSoutheast = "sdb.ap-southeast-1.amazonaws.com"

sdbApNortheast :: B.ByteString
sdbApNortheast = "sdb.ap-northeast-1.amazonaws.com"

sdbHttpGet :: B.ByteString -> SdbConfiguration qt
sdbHttpGet endpoint = SdbConfiguration HTTP Get endpoint (defaultPort HTTP)

sdbHttpPost :: B.ByteString -> SdbConfiguration NormalQuery
sdbHttpPost endpoint = SdbConfiguration HTTP PostQuery endpoint (defaultPort HTTP)

sdbHttpsGet :: B.ByteString -> SdbConfiguration qt
sdbHttpsGet endpoint = SdbConfiguration HTTPS Get endpoint (defaultPort HTTPS)

sdbHttpsPost :: B.ByteString -> SdbConfiguration NormalQuery
sdbHttpsPost endpoint = SdbConfiguration HTTPS PostQuery endpoint (defaultPort HTTPS)

sdbSignQuery :: [(B.ByteString, B.ByteString)] -> SdbConfiguration qt -> SignatureData -> SignedQuery
sdbSignQuery q si sd
    = SignedQuery {
        sqMethod = method
      , sqProtocol = sdbiProtocol si
      , sqHost = host
      , sqPort = sdbiPort si
      , sqPath = path
      , sqQuery = sq
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType = Nothing
      , sqContentMd5 = Nothing
      , sqAmzHeaders = []
      , sqOtherHeaders = []
      , sqBody = Nothing
      , sqStringToSign = stringToSign
      }
    where
      ah = HmacSHA256
      q' = HTTP.toQuery . sort $ q ++ ("Version", "2009-04-15") : queryAuth
      ti = signatureTimeInfo sd
      cr = signatureCredentials sd
      queryAuth = [case ti of
                     AbsoluteTimestamp time -> ("Timestamp", fmtAmzTime time)
                     AbsoluteExpires   time -> ("Expires", fmtAmzTime time)
                  , ("AWSAccessKeyId", accessKeyID cr)
                  , ("SignatureMethod", amzHash ah)
                  , ("SignatureVersion", "2")]
                  ++ maybe [] (\tok -> [("SecurityToken", tok)]) (iamToken cr)
      sq = ("Signature", Just sig) : q'
      method = sdbiHttpMethod si
      host = sdbiHost si
      path = "/"
      sig = signature cr ah stringToSign
      stringToSign = Blaze.toByteString . mconcat $
                     intersperse (Blaze8.fromChar '\n')
                       [Blaze.copyByteString $ httpMethod method
                       , Blaze.copyByteString $ host
                       , Blaze.copyByteString $ path
                       , HTTP.renderQueryBuilder False q']

sdbResponseConsumer :: (Cu.Cursor -> Response SdbMetadata a)
                    -> IORef SdbMetadata
                    -> HTTPResponseConsumer a
sdbResponseConsumer inner metadataRef resp
    = xmlCursorConsumer parse metadataRef resp
    where parse cursor
              = do let requestId' = listToMaybe $ cursor $// elContent "RequestID"
                   let boxUsage' = listToMaybe $ cursor $// elContent "BoxUsage"
                   tellMetadata $ SdbMetadata requestId' boxUsage'
                   case cursor $// Cu.laxElement "Error" of
                     []      -> inner cursor
                     (err:_) -> fromError err
          fromError cursor = do errCode <- force "Missing Error Code" $ cursor $// elCont "Code"
                                errMessage <- force "Missing Error Message" $ cursor $// elCont "Message"
                                throwM $ SdbError (HTTP.responseStatus resp) errCode errMessage

class SdbFromResponse a where
    sdbFromResponse :: Cu.Cursor -> Response SdbMetadata a

sdbCheckResponseType :: MonadThrow m => a -> T.Text -> Cu.Cursor -> m a
sdbCheckResponseType a n c = do _ <- force ("Expected response type " ++ T.unpack n) (Cu.laxElement n c)
                                return a

decodeBase64 :: MonadThrow m => Cu.Cursor -> m T.Text
decodeBase64 cursor =
  let encoded = T.concat $ cursor $/ Cu.content
      encoding = listToMaybe $ cursor $| Cu.laxAttribute "encoding" &| T.toCaseFold
  in
    case encoding of
      Nothing -> return encoded
      Just "base64" -> case Base64.decode . T.encodeUtf8 $ encoded of
                         Left msg -> throwM $ XmlException ("Invalid Base64 data: " ++ msg)
                         Right x -> return $ T.decodeUtf8 x
      Just actual -> throwM $ XmlException ("Unrecognized encoding " ++ T.unpack actual)

data Attribute a
    = ForAttribute { attributeName :: T.Text, attributeData :: a }
    deriving (Show)

readAttribute :: MonadThrow m => Cu.Cursor -> m (Attribute T.Text)
readAttribute cursor = do
  name <- forceM "Missing Name" $ cursor $/ Cu.laxElement "Name" &| decodeBase64
  value <- forceM "Missing Value" $ cursor $/ Cu.laxElement "Value" &| decodeBase64
  return $ ForAttribute name value

data SetAttribute
    = SetAttribute { setAttribute :: T.Text, isReplaceAttribute :: Bool }
    deriving (Show)

attributeQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Attribute a -> [(B.ByteString, B.ByteString)]
attributeQuery  f (ForAttribute name x) =  ("Name", T.encodeUtf8 name) : f x

addAttribute :: T.Text -> T.Text -> Attribute SetAttribute
addAttribute name value = ForAttribute name (SetAttribute value False)

replaceAttribute :: T.Text -> T.Text -> Attribute SetAttribute
replaceAttribute name value = ForAttribute name (SetAttribute value True)

setAttributeQuery :: SetAttribute -> [(B.ByteString, B.ByteString)]
setAttributeQuery (SetAttribute value replace)
    = ("Value", T.encodeUtf8 value) : [("Replace", awsTrue) | replace]

data DeleteAttribute
    = DeleteAttribute
    | ValuedDeleteAttribute { deleteAttributeValue :: T.Text }
    deriving (Show)

deleteAttributeQuery :: DeleteAttribute -> [(B.ByteString, B.ByteString)]
deleteAttributeQuery DeleteAttribute = []
deleteAttributeQuery (ValuedDeleteAttribute value) = [("Value", T.encodeUtf8 value)]

data ExpectedAttribute
    = ExpectedValue { expectedAttributeValue :: T.Text }
    | ExpectedExists { expectedAttributeExists :: Bool }
    deriving (Show)

expectedValue :: T.Text -> T.Text -> Attribute ExpectedAttribute
expectedValue name value = ForAttribute name (ExpectedValue value)

expectedExists :: T.Text -> Bool -> Attribute ExpectedAttribute
expectedExists name exists = ForAttribute name (ExpectedExists exists)

expectedAttributeQuery :: ExpectedAttribute -> [(B.ByteString, B.ByteString)]
expectedAttributeQuery (ExpectedValue value) = [("Value", T.encodeUtf8 value)]
expectedAttributeQuery (ExpectedExists exists) = [("Exists", awsBool exists)]

data Item a
    = Item { itemName :: T.Text, itemData :: a }
    deriving (Show)

readItem :: MonadThrow m => Cu.Cursor -> m (Item [Attribute T.Text])
readItem cursor = do
  name <- force "Missing Name" <=< sequence $ cursor $/ Cu.laxElement "Name" &| decodeBase64
  attributes <- sequence $ cursor $/ Cu.laxElement "Attribute" &| readAttribute
  return $ Item name attributes

itemQuery :: (a -> [(B.ByteString, B.ByteString)]) -> Item a -> [(B.ByteString, B.ByteString)]
itemQuery f (Item name x) = ("ItemName", T.encodeUtf8 name) : f x
