module Aws.Ses.Commands.GetIdentityNotificationAttributes
    ( GetIdentityNotificationAttributes(..)
    , GetIdentityNotificationAttributesResponse(..)
    , IdentityNotificationAttributes(..)
    ) where

import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import Control.Applicative ((<$>))
import Data.Text.Encoding as T (encodeUtf8)
import Data.Text as T (toCaseFold)
import Data.Typeable
import Text.XML.Cursor (($//), ($/), (&|), laxElement)

import Aws.Core
import Aws.Ses.Core

-- | Get notification settings for the given identities.
data GetIdentityNotificationAttributes = GetIdentityNotificationAttributes [Text]
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery GetIdentityNotificationAttributes where
    type ServiceConfiguration GetIdentityNotificationAttributes = SesConfiguration
    signQuery (GetIdentityNotificationAttributes identities) =
        sesSignQuery $ ("Action", "GetIdentityNotificationAttributes")
                     : zip (enumMember <$> [1..]) (T.encodeUtf8 <$> identities)
            where enumMember (n :: Int) = BS.append "Identities.member." (BS.pack $ show n)

data IdentityNotificationAttributes = IdentityNotificationAttributes
    { inIdentity          :: Text
    , inBounceTopic       :: Maybe Text
    , inComplaintTopic    :: Maybe Text
    , inForwardingEnabled :: Bool
    }
    deriving (Eq, Ord, Show, Typeable)

-- | The response sent back by Amazon SES after a
-- 'GetIdentityNotificationAttributes' command.
data GetIdentityNotificationAttributesResponse =
    GetIdentityNotificationAttributesResponse [IdentityNotificationAttributes]
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer GetIdentityNotificationAttributes GetIdentityNotificationAttributesResponse where
    type ResponseMetadata GetIdentityNotificationAttributesResponse = SesMetadata
    responseConsumer _ = sesResponseConsumer $ \cursor -> do
        let buildAttr e = do
              inIdentity <- force "Missing Key" $ e $/ elContent "key"
              fwdText <- force "Missing ForwardingEnabled" $ e $// elContent "ForwardingEnabled"
              let inBounceTopic       = headOrNothing (e $// elContent "BounceTopic")
                  inComplaintTopic    = headOrNothing (e $// elContent "ComplaintTopic")
                  inForwardingEnabled = T.toCaseFold fwdText == T.toCaseFold "true"
              return IdentityNotificationAttributes{..}
        attributes <- sequence $ cursor $// laxElement "entry" &| buildAttr
        return $ GetIdentityNotificationAttributesResponse attributes
      where
        headOrNothing (x:_) = Just x
        headOrNothing    _  = Nothing

instance Transaction GetIdentityNotificationAttributes GetIdentityNotificationAttributesResponse where

instance AsMemoryResponse GetIdentityNotificationAttributesResponse where
    type MemoryResponse GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse
    loadToMemory = return
