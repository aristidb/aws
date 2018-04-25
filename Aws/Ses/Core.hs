module Aws.Ses.Core
    ( SesError(..)
    , SesMetadata(..)

    , SesConfiguration(..)
    , sesEuWest1
    , sesUsEast
    , sesUsEast1
    , sesUsWest2
    , sesHttpsGet
    , sesHttpsPost

    , sesSignQuery

    , sesResponseConsumer

    , RawMessage(..)
    , Destination(..)
    , EmailAddress
    , Sender(..)
    , sesAsQuery
    ) where

import           Aws.Core
import qualified Blaze.ByteString.Builder       as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8
import qualified Control.Exception              as C
import           Control.Monad                  (mplus)
import           Control.Monad.Trans.Resource   (throwM)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base64         as B64
import           Data.ByteString.Char8          ({-IsString-})
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import qualified Data.Semigroup                 as Sem
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as TE
import           Data.Typeable
import           Prelude
import qualified Network.HTTP.Conduit           as HTTP
import qualified Network.HTTP.Types             as HTTP
import           Text.XML.Cursor                (($/), ($//))
import qualified Text.XML.Cursor                as Cu

data SesError
    = SesError {
        sesStatusCode   :: HTTP.Status
      , sesErrorCode    :: Text
      , sesErrorMessage :: Text
      }
    deriving (Show, Typeable)

instance C.Exception SesError

data SesMetadata
    = SesMetadata {
        requestId :: Maybe Text
      }
    deriving (Show, Typeable)

instance Loggable SesMetadata where
    toLogText (SesMetadata rid) = "SES: request ID=" `mappend` fromMaybe "<none>" rid

instance Sem.Semigroup SesMetadata where
    SesMetadata r1 <> SesMetadata r2 = SesMetadata (r1 `mplus` r2)

instance Monoid SesMetadata where
    mempty = SesMetadata Nothing
    mappend = (Sem.<>)

data SesConfiguration qt
    = SesConfiguration {
        sesiHttpMethod :: Method
      , sesiHost       :: B.ByteString
      }
    deriving (Show)

-- HTTP is not supported right now, always use HTTPS
instance DefaultServiceConfiguration (SesConfiguration NormalQuery) where
    defServiceConfig = sesHttpsPost sesUsEast1

instance DefaultServiceConfiguration (SesConfiguration UriOnlyQuery) where
    defServiceConfig = sesHttpsGet sesUsEast1

sesEuWest1 :: B.ByteString
sesEuWest1 = "email.eu-west-1.amazonaws.com"

sesUsEast :: B.ByteString
sesUsEast = sesUsEast1

sesUsEast1 :: B.ByteString
sesUsEast1 = "email.us-east-1.amazonaws.com"

sesUsWest2 :: B.ByteString
sesUsWest2 = "email.us-west-2.amazonaws.com"

sesHttpsGet :: B.ByteString -> SesConfiguration qt
sesHttpsGet endpoint = SesConfiguration Get endpoint

sesHttpsPost :: B.ByteString -> SesConfiguration NormalQuery
sesHttpsPost endpoint = SesConfiguration PostQuery endpoint

sesSignQuery :: [(B.ByteString, B.ByteString)] -> SesConfiguration qt -> SignatureData -> SignedQuery
sesSignQuery query si sd
    = SignedQuery {
        sqMethod        = sesiHttpMethod si
      , sqProtocol      = HTTPS
      , sqHost          = sesiHost si
      , sqPort          = defaultPort HTTPS
      , sqPath          = "/"
      , sqQuery         = HTTP.simpleQueryToQuery query'
      , sqDate          = Just $ signatureTime sd
      , sqAuthorization = Nothing
      , sqContentType   = Nothing
      , sqContentMd5    = Nothing
      , sqAmzHeaders    = amzHeaders
      , sqOtherHeaders  = []
      , sqBody          = Nothing
      , sqStringToSign  = stringToSign
      }
    where
      stringToSign  = fmtRfc822Time (signatureTime sd)
      credentials   = signatureCredentials sd
      accessKeyId   = accessKeyID credentials
      amzHeaders    = catMaybes
                    [ Just ("X-Amzn-Authorization", authorization)
                    , ("x-amz-security-token",) `fmap` iamToken credentials
                    ]
      authorization = B.concat
                    [ "AWS3-HTTPS AWSAccessKeyId="
                    , accessKeyId
                    , ", Algorithm=HmacSHA256, Signature="
                    , signature credentials HmacSHA256 stringToSign
                    ]
      query' = ("AWSAccessKeyId", accessKeyId) : query

sesResponseConsumer :: (Cu.Cursor -> Response SesMetadata a)
                    -> IORef SesMetadata
                    -> HTTPResponseConsumer a
sesResponseConsumer inner metadataRef resp = xmlCursorConsumer parse metadataRef resp
    where
      parse cursor = do
        let requestId' = listToMaybe $ cursor $// elContent "RequestID"
        tellMetadata $ SesMetadata requestId'
        case cursor $/ Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err

      fromError cursor = do
        errCode    <- force "Missing Error Code"    $ cursor $// elContent "Code"
        errMessage <- force "Missing Error Message" $ cursor $// elContent "Message"
        throwM $ SesError (HTTP.responseStatus resp) errCode errMessage

class SesAsQuery a where
    -- | Write a data type as a list of query parameters.
    sesAsQuery :: a -> [(B.ByteString, B.ByteString)]

instance SesAsQuery a => SesAsQuery (Maybe a) where
    sesAsQuery = maybe [] sesAsQuery


-- | A raw e-mail.
data RawMessage = RawMessage { rawMessageData :: B.ByteString }
                deriving (Eq, Ord, Show, Typeable)

instance SesAsQuery RawMessage where
    sesAsQuery = (:[]) . (,) "RawMessage.Data" . B64.encode . rawMessageData


-- | The destinations of an e-mail.
data Destination =
    Destination
      { destinationBccAddresses :: [EmailAddress]
      , destinationCcAddresses  :: [EmailAddress]
      , destinationToAddresses  :: [EmailAddress]
      } deriving (Eq, Ord, Show, Typeable)

instance SesAsQuery Destination where
    sesAsQuery (Destination bcc cc to) = concat [ go (s "Bcc") bcc
                                                , go (s "Cc")  cc
                                                , go (s "To")  to ]
        where
          go kind = zipWith f (map Blaze8.fromShow [one..])
              where txt = kind `mappend` s "Addresses.member."
                    f n v = ( Blaze.toByteString (txt `mappend` n)
                            , TE.encodeUtf8 v )
          s = Blaze.fromByteString
          one = 1 :: Int

instance Sem.Semigroup Destination where
    (Destination a1 a2 a3) <> (Destination b1 b2 b3) =
        Destination (a1 ++ b1) (a2 ++ b2) (a3 ++ b3)

instance Monoid Destination where
    mempty = Destination [] [] []
    mappend = (Sem.<>)

-- | An e-mail address.
type EmailAddress = Text


-- | The sender's e-mail address.
data Sender = Sender { senderAddress :: EmailAddress }
              deriving (Eq, Ord, Show, Typeable)

instance SesAsQuery Sender where
    sesAsQuery = (:[]) . (,) "Source" . TE.encodeUtf8 . senderAddress
