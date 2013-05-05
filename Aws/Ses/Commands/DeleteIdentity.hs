module Aws.Ses.Commands.DeleteIdentity
    ( DeleteIdentity(..)
    , DeleteIdentityResponse(..)
    ) where

import Data.Text (Text)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Aws.Core
import Aws.Ses.Core

-- | Delete an email address or domain
data DeleteIdentity  = DeleteIdentity Text
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery DeleteIdentity where
    type ServiceConfiguration DeleteIdentity = SesConfiguration
    signQuery (DeleteIdentity identity) =
        sesSignQuery [ ("Action", "DeleteIdentity")
                     , ("Identity", T.encodeUtf8 identity)
                     ]

-- | The response sent back by Amazon SES after a
-- 'DeleteIdentity' command.
data DeleteIdentityResponse = DeleteIdentityResponse
    deriving (Eq, Ord, Show, Typeable)


instance ResponseConsumer DeleteIdentity DeleteIdentityResponse where
    type ResponseMetadata DeleteIdentityResponse = SesMetadata
    responseConsumer _ = sesResponseConsumer $ \_ -> return DeleteIdentityResponse


instance Transaction DeleteIdentity DeleteIdentityResponse where

instance AsMemoryResponse DeleteIdentityResponse where
    type MemoryResponse DeleteIdentityResponse = DeleteIdentityResponse
    loadToMemory = return
