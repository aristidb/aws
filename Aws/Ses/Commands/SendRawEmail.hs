{-# LANGUAGE DeriveDataTypeable, RecordWildCards, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.Ses.Commands.SendRawEmail
    ( SendRawEmail(..)
    , SendRawEmailResponse(..)
    ) where

import Data.Text (Text)
import Data.Typeable
import Text.XML.Cursor (($//))

import Aws.Signature
import Aws.Response
import Aws.Transaction
import Aws.Xml
import Aws.Ses.Info
import Aws.Ses.Query
import Aws.Ses.Metadata
import Aws.Ses.Model
import Aws.Ses.Response

-- | Send a raw e-mail message.
data SendRawEmail =
    SendRawEmail
      { srmDestinations :: Maybe Destination
      , srmRawMessage   :: RawMessage
      , srmSource       :: Maybe Sender
      }
    deriving (Eq, Ord, Show, Typeable)

instance SignQuery SendRawEmail where
    type Info SendRawEmail = SesInfo
    signQuery SendRawEmail {..} =
        sesSignQuery $ ("Action", "SendRawEmail") :
                       concat [ sesAsQuery srmDestinations
                              , sesAsQuery srmRawMessage
                              , sesAsQuery srmSource
                              ]

-- | The response sent back by Amazon SES after a
-- 'SendRawEmail' command.
data SendRawEmailResponse =
    SendRawEmailResponse { srmrMessageId :: Text }
    deriving (Eq, Ord, Show, Typeable)


instance ResponseConsumer SendRawEmail SendRawEmailResponse where
    type ResponseMetadata SendRawEmailResponse = SesMetadata
    responseConsumer _ =
      sesResponseConsumer $ \cursor -> do
        messageId <- force "MessageId not found" $ cursor $// elContent "MessageId"
        return (SendRawEmailResponse messageId)


instance Transaction SendRawEmail SendRawEmailResponse where
