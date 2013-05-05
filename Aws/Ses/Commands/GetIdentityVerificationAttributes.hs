module Aws.Ses.Commands.GetIdentityVerificationAttributes
    ( GetIdentityVerificationAttributes(..)
    , GetIdentityVerificationAttributesResponse(..)
    , IdentityVerificationAttributes(..)
    ) where

import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (listToMaybe)
import Control.Applicative ((<$>))
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Text.XML.Cursor (($//), ($/), (&|), laxElement)

import Aws.Core
import Aws.Ses.Core

-- | Get verification status for a list of email addresses and/or domains
data GetIdentityVerificationAttributes = GetIdentityVerificationAttributes [Text]
    deriving (Eq, Ord, Show, Typeable)


-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery GetIdentityVerificationAttributes where
    type ServiceConfiguration GetIdentityVerificationAttributes = SesConfiguration
    signQuery (GetIdentityVerificationAttributes identities) =
        sesSignQuery $ ("Action", "GetIdentityVerificationAttributes")
                     : zip (enumMember <$> [1..]) (T.encodeUtf8 <$> identities)
            where enumMember (n :: Int) = BS.append "Identities.member." (BS.pack $ show n)

data IdentityVerificationAttributes = IdentityVerificationAttributes
    { ivIdentity :: Text
    , ivVerificationStatus :: Text
    , ivVerificationToken :: Maybe Text
    }
    deriving (Eq, Ord, Show, Typeable)


-- | The response sent back by Amazon SES after a
-- 'GetIdentityVerificationAttributes' command.
data GetIdentityVerificationAttributesResponse =
    GetIdentityVerificationAttributesResponse [IdentityVerificationAttributes]
    deriving (Eq, Ord, Show, Typeable)


instance ResponseConsumer GetIdentityVerificationAttributes GetIdentityVerificationAttributesResponse where
    type ResponseMetadata GetIdentityVerificationAttributesResponse = SesMetadata
    responseConsumer _ =
      sesResponseConsumer $ \cursor -> do
         let buildAttr e = do
               ivIdentity <- force "Missing Key" $ e $/ elContent "key"
               ivVerificationStatus <- force "Missing Verification Status" $ e
                   $// elContent "VerificationStatus"
               let ivVerificationToken = listToMaybe $ e $// elContent "VerificationToken"
               return IdentityVerificationAttributes {..}
         attributes <- sequence $ cursor $// laxElement "entry" &| buildAttr
         return $ GetIdentityVerificationAttributesResponse attributes


instance Transaction GetIdentityVerificationAttributes GetIdentityVerificationAttributesResponse where

instance AsMemoryResponse GetIdentityVerificationAttributesResponse where
    type MemoryResponse GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse
    loadToMemory = return
