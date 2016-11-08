module Aws.Ses.Commands.VerifyDomainIdentity
    ( VerifyDomainIdentity(..)
    , VerifyDomainIdentityResponse(..)
    ) where

import Data.Text (Text)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Aws.Core
import Aws.Ses.Core
import Text.XML.Cursor (($//))

-- | Verify ownership of a domain.
data VerifyDomainIdentity  = VerifyDomainIdentity Text
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery VerifyDomainIdentity where
    type ServiceConfiguration VerifyDomainIdentity = SesConfiguration
    signQuery (VerifyDomainIdentity domain) =
        sesSignQuery [ ("Action", "VerifyDomainIdentity")
                     , ("Domain", T.encodeUtf8 domain)
                     ]

-- | The response sent back by Amazon SES after a
-- 'VerifyDomainIdentity' command.
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse Text
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer VerifyDomainIdentity VerifyDomainIdentityResponse where
    type ResponseMetadata VerifyDomainIdentityResponse = SesMetadata
    responseConsumer _ _ =
      sesResponseConsumer $ \cursor -> do
        token <- force "Verification token not found" $ cursor $// elContent "VerificationToken"
        return (VerifyDomainIdentityResponse token)

instance Transaction VerifyDomainIdentity VerifyDomainIdentityResponse where

instance AsMemoryResponse VerifyDomainIdentityResponse where
    type MemoryResponse VerifyDomainIdentityResponse = VerifyDomainIdentityResponse
    loadToMemory = return
