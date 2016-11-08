module Aws.Ses.Commands.SetIdentityDkimEnabled
    ( SetIdentityDkimEnabled(..)
    , SetIdentityDkimEnabledResponse(..)
    ) where

import           Aws.Core
import           Aws.Ses.Core
import           Data.Text          (Text)
import           Data.Text.Encoding as T
import           Data.Typeable

-- | Change whether bounces and complaints for the given identity will be
-- DKIM signed.
data SetIdentityDkimEnabled = SetIdentityDkimEnabled
      { sdDkimEnabled :: Bool
      , sdIdentity    :: Text
      }
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery SetIdentityDkimEnabled where
    type ServiceConfiguration SetIdentityDkimEnabled = SesConfiguration
    signQuery SetIdentityDkimEnabled{..} =
        sesSignQuery [ ("Action",   "SetIdentityDkimEnabled")
                     , ("Identity",  T.encodeUtf8 sdIdentity)
                     , ("DkimEnabled", awsBool sdDkimEnabled)
                     ]

-- | The response sent back by SES after the 'SetIdentityDkimEnabled' command.
data SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer SetIdentityDkimEnabled SetIdentityDkimEnabledResponse where
    type ResponseMetadata SetIdentityDkimEnabledResponse = SesMetadata
    responseConsumer _ _
        = sesResponseConsumer $ \_ -> return SetIdentityDkimEnabledResponse

instance Transaction SetIdentityDkimEnabled SetIdentityDkimEnabledResponse

instance AsMemoryResponse SetIdentityDkimEnabledResponse where
    type MemoryResponse SetIdentityDkimEnabledResponse = SetIdentityDkimEnabledResponse
    loadToMemory = return
