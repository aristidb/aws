module Aws.Ses.Commands.SetIdentityFeedbackForwardingEnabled
    ( SetIdentityFeedbackForwardingEnabled(..)
    , SetIdentityFeedbackForwardingEnabledResponse(..)
    ) where

import Data.Text (Text)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Aws.Core
import Aws.Ses.Core

-- | Change whether bounces and complaints for the given identity will be
-- forwarded as email.
data SetIdentityFeedbackForwardingEnabled =
    SetIdentityFeedbackForwardingEnabled
      { sffForwardingEnabled :: Bool
      , sffIdentity          :: Text
      }
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery SetIdentityFeedbackForwardingEnabled where
    type ServiceConfiguration SetIdentityFeedbackForwardingEnabled = SesConfiguration
    signQuery SetIdentityFeedbackForwardingEnabled{..} =
        sesSignQuery [ ("Action",  "SetIdentityFeedbackForwardingEnabled")
                     , ("Identity",              T.encodeUtf8 sffIdentity)
                     , ("ForwardingEnabled", awsBool sffForwardingEnabled)
                     ]

-- | The response sent back by SES after the
-- 'SetIdentityFeedbackForwardingEnabled' command.
data SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer SetIdentityFeedbackForwardingEnabled SetIdentityFeedbackForwardingEnabledResponse where
    type ResponseMetadata SetIdentityFeedbackForwardingEnabledResponse = SesMetadata
    responseConsumer _ _
        = sesResponseConsumer $ \_ -> return SetIdentityFeedbackForwardingEnabledResponse

instance Transaction SetIdentityFeedbackForwardingEnabled SetIdentityFeedbackForwardingEnabledResponse

instance AsMemoryResponse SetIdentityFeedbackForwardingEnabledResponse where
    type MemoryResponse SetIdentityFeedbackForwardingEnabledResponse = SetIdentityFeedbackForwardingEnabledResponse
    loadToMemory = return
