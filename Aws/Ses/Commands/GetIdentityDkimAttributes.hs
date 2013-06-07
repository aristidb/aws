module Aws.Ses.Commands.GetIdentityDkimAttributes
    ( GetIdentityDkimAttributes(..)
    , GetIdentityDkimAttributesResponse(..)
    , IdentityDkimAttributes(..)
    ) where

import           Control.Applicative   ((<$>))
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import           Data.Text             as T (toCaseFold)
import           Data.Text.Encoding    as T (encodeUtf8)
import           Data.Typeable
import           Text.XML.Cursor       (laxElement, ($/), ($//), (&/), (&|))

import           Aws.Core
import           Aws.Ses.Core

-- | Get notification settings for the given identities.
data GetIdentityDkimAttributes = GetIdentityDkimAttributes [Text]
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery GetIdentityDkimAttributes where
    type ServiceConfiguration GetIdentityDkimAttributes = SesConfiguration
    signQuery (GetIdentityDkimAttributes identities) =
        sesSignQuery $ ("Action", "GetIdentityDkimAttributes")
                     : zip (enumMember <$> [1..]) (T.encodeUtf8 <$> identities)
            where enumMember (n :: Int) = BS.append "Identities.member." (BS.pack $ show n)


data IdentityDkimAttributes =
    IdentityDkimAttributes
      { idIdentity                :: Text
      , idDkimEnabled             :: Bool
      , idDkimTokens              :: [Text]
      , idDkimVerirficationStatus :: Text }
    deriving (Eq, Ord, Show, Typeable)

-- | The response sent back by Amazon SES after a
-- 'GetIdentityDkimAttributes' command.
data GetIdentityDkimAttributesResponse =
    GetIdentityDkimAttributesResponse [IdentityDkimAttributes]
    deriving (Eq, Ord, Show, Typeable)

instance ResponseConsumer GetIdentityDkimAttributes GetIdentityDkimAttributesResponse where
    type ResponseMetadata GetIdentityDkimAttributesResponse = SesMetadata
    responseConsumer _ = sesResponseConsumer $ \cursor -> do
        let buildAttr e = do
              idIdentity <- force "Missing Key" $ e $/ elContent "key"
              enabled <- force "Missing DkimEnabled" $ e $// elContent "DkimEnabled"
              idDkimVerirficationStatus <- force "Missing status" $
                                           e $// elContent "DkimVerificationStatus"
              let idDkimEnabled = T.toCaseFold enabled == T.toCaseFold "true"
                  idDkimTokens = e $// laxElement "DkimTokens" &/ elContent "member"
              return IdentityDkimAttributes{..}
        attributes <- sequence $ cursor $// laxElement "entry" &| buildAttr
        return $ GetIdentityDkimAttributesResponse attributes

instance Transaction GetIdentityDkimAttributes GetIdentityDkimAttributesResponse where

instance AsMemoryResponse GetIdentityDkimAttributesResponse where
    type MemoryResponse GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse
    loadToMemory = return
