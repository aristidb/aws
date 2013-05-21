module Aws.Ses.Commands.SetIdentityNotificationTopic
    ( SetIdentityNotificationTopic(..)
    , SetIdentityNotificationTopicResponse(..)
    , NotificationType(..)
    ) where

import Data.Text (Text)
import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Aws.Core
import Aws.Ses.Core

data NotificationType = Bounce | Complaint
    deriving (Eq, Ord, Show, Typeable)

-- | Change or remove the Amazon SNS notification topic to which notification
-- of the given type are published.
data SetIdentityNotificationTopic =
    SetIdentityNotificationTopic
      { sntIdentity         :: Text
      -- ^ The identity for which the SNS topic will be changed.
      , sntNotificationType :: NotificationType
      -- ^ The type of notifications that will be published to the topic.
      , sntSnsTopic         :: Maybe Text
      -- ^ @Just@ the ARN of the SNS topic or @Nothing@ to unset the topic.
      }
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery SetIdentityNotificationTopic where
    type ServiceConfiguration SetIdentityNotificationTopic = SesConfiguration
    signQuery SetIdentityNotificationTopic{..} =
        let notificationType = case sntNotificationType of
                                  Bounce    -> "Bounce"
                                  Complaint -> "Complaint"
            snsTopic = ("SnsTopic",) . T.encodeUtf8 <$> sntSnsTopic
        in sesSignQuery $ [ ("Action", "SetIdentityNotificationTopic")
                          , ("Identity",     T.encodeUtf8 sntIdentity)
                          , ("NotificationType",     notificationType)
                          ] ++ maybeToList snsTopic

-- | The response sent back by SES after the 'SetIdentityNotificationTopic'
-- command.
data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer SetIdentityNotificationTopic SetIdentityNotificationTopicResponse where
    type ResponseMetadata SetIdentityNotificationTopicResponse = SesMetadata
    responseConsumer _ = sesResponseConsumer $ \_ -> return SetIdentityNotificationTopicResponse 

instance Transaction SetIdentityNotificationTopic SetIdentityNotificationTopicResponse

instance AsMemoryResponse SetIdentityNotificationTopicResponse where
    type MemoryResponse SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse
    loadToMemory = return
