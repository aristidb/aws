module Aws.Ses.Commands.VerifyDomainDkim
    ( VerifyDomainDkim(..)
    , VerifyDomainDkimResponse(..)
    ) where

import Data.Text (Text)
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Aws.Core
import Aws.Ses.Core
import Text.XML.Cursor (($//), laxElement, (&/))

-- | Verify ownership of a domain.
data VerifyDomainDkim  = VerifyDomainDkim Text
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery VerifyDomainDkim where
    type ServiceConfiguration VerifyDomainDkim = SesConfiguration
    signQuery (VerifyDomainDkim domain) =
        sesSignQuery [ ("Action", "VerifyDomainDkim")
                     , ("Domain", T.encodeUtf8 domain)
                     ]

-- | The response sent back by Amazon SES after a 'VerifyDomainDkim' command.
data VerifyDomainDkimResponse = VerifyDomainDkimResponse [Text]
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer VerifyDomainDkim VerifyDomainDkimResponse where
    type ResponseMetadata VerifyDomainDkimResponse = SesMetadata
    responseConsumer _ _ =
      sesResponseConsumer $ \cursor -> do
        let tokens = cursor $// laxElement "DkimTokens" &/ elContent "member"
        return (VerifyDomainDkimResponse tokens)

instance Transaction VerifyDomainDkim VerifyDomainDkimResponse where

instance AsMemoryResponse VerifyDomainDkimResponse where
    type MemoryResponse VerifyDomainDkimResponse = VerifyDomainDkimResponse
    loadToMemory = return
