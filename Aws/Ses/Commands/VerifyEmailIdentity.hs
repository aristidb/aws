module Aws.Ses.Commands.VerifyEmailIdentity
    ( VerifyEmailIdentity(..)
    , VerifyEmailIdentityResponse(..)
    ) where

import Data.Text (Text)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Aws.Core
import Aws.Ses.Core

-- | List email addresses and/or domains
data VerifyEmailIdentity  = VerifyEmailIdentity Text
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery VerifyEmailIdentity where
    type ServiceConfiguration VerifyEmailIdentity = SesConfiguration
    signQuery (VerifyEmailIdentity address) =
        sesSignQuery [ ("Action", "VerifyEmailIdentity")
                     , ("EmailAddress", T.encodeUtf8 address)
                     ]

-- | The response sent back by Amazon SES after a
-- 'VerifyEmailIdentity' command.
data VerifyEmailIdentityResponse = VerifyEmailIdentityResponse
    deriving (Eq, Ord, Show, Typeable)


instance ResponseConsumer VerifyEmailIdentity VerifyEmailIdentityResponse where
    type ResponseMetadata VerifyEmailIdentityResponse = SesMetadata
    responseConsumer _ = sesResponseConsumer $ \_ -> return VerifyEmailIdentityResponse


instance Transaction VerifyEmailIdentity VerifyEmailIdentityResponse where

instance AsMemoryResponse VerifyEmailIdentityResponse where
    type MemoryResponse VerifyEmailIdentityResponse = VerifyEmailIdentityResponse
    loadToMemory = return
