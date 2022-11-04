module Aws.Ses.Commands.SendRawEmail
    ( SendRawEmail(..)
    , SendRawEmailResponse(..)
    ) where

import Data.Text (Text)
import Data.Typeable
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Text.XML.Cursor (($//))
import qualified Data.Text.Encoding as T
import Prelude

import Aws.Core
import Aws.Ses.Core

-- | Send a raw e-mail message.
data SendRawEmail =
    SendRawEmail
      { srmDestinations :: [EmailAddress]
      , srmRawMessage   :: RawMessage
      , srmSource       :: Maybe Sender
      }
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery SendRawEmail where
    type ServiceConfiguration SendRawEmail = SesConfiguration
    signQuery SendRawEmail {..} =
        sesSignQuery $ ("Action", "SendRawEmail") :
                       concat [ destinations
                              , sesAsQuery srmRawMessage
                              , sesAsQuery srmSource
                              ]
      where
        destinations = zip (enumMember   <$> ([1..] :: [Int]))
                           (T.encodeUtf8 <$>  srmDestinations)
        enumMember   = BS.append "Destinations.member." . BS.pack . show

-- | The response sent back by Amazon SES after a
-- 'SendRawEmail' command.
data SendRawEmailResponse =
    SendRawEmailResponse { srmrMessageId :: Text }
    deriving (Eq, Ord, Show, Typeable)


instance ResponseConsumer SendRawEmail SendRawEmailResponse where
    type ResponseMetadata SendRawEmailResponse = SesMetadata
    responseConsumer _ _ =
      sesResponseConsumer $ \cursor -> do
        messageId <- force "MessageId not found" $ cursor $// elContent "MessageId"
        return (SendRawEmailResponse messageId)


instance Transaction SendRawEmail SendRawEmailResponse where

instance AsMemoryResponse SendRawEmailResponse where
    type MemoryResponse SendRawEmailResponse = SendRawEmailResponse
    loadToMemory = return
