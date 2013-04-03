module Aws.Ses.Commands.ListIdentities
    ( ListIdentities(..)
    , ListIdentitiesResponse(..)
    , IdentityType(..)
    ) where

import Data.Text (Text)
import  qualified Data.ByteString.Char8 as BS
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import Data.Text.Encoding as T (encodeUtf8)
import Data.Typeable
import Text.XML.Cursor (($//), (&/), laxElement)

import Aws.Core
import Aws.Ses.Core

-- | List email addresses and/or domains
data ListIdentities =
    ListIdentities
      { liIdentityType :: Maybe IdentityType
      , liMaxItems :: Maybe Int -- valid range is 1..100
      , liNextToken :: Maybe Text
      }
    deriving (Eq, Ord, Show, Typeable)

data IdentityType = EmailAddress | Domain
    deriving (Eq, Ord, Show, Typeable)

-- | ServiceConfiguration: 'SesConfiguration'
instance SignQuery ListIdentities where
    type ServiceConfiguration ListIdentities = SesConfiguration
    signQuery ListIdentities {..} =
        let it = case liIdentityType of
                     Just EmailAddress -> Just "EmailAddress"
                     Just Domain -> Just "Domain"
                     Nothing -> Nothing
        in sesSignQuery $ ("Action", "ListIdentities")
                          : catMaybes
                          [ ("IdentityType",) <$> it
                          , ("MaxItems",) . BS.pack . show <$> liMaxItems
                          , ("NextToken",) . T.encodeUtf8 <$> liNextToken
                          ]

-- | The response sent back by Amazon SES after a
-- 'ListIdentities' command.
data ListIdentitiesResponse = ListIdentitiesResponse [Text]
    deriving (Eq, Ord, Show, Typeable)


instance ResponseConsumer ListIdentities ListIdentitiesResponse where
    type ResponseMetadata ListIdentitiesResponse = SesMetadata
    responseConsumer _ =
      sesResponseConsumer $ \cursor -> do
         let ids = cursor $// laxElement "Identities" &/ elContent "member"
         return $ ListIdentitiesResponse ids


instance Transaction ListIdentities ListIdentitiesResponse where

instance AsMemoryResponse ListIdentitiesResponse where
    type MemoryResponse ListIdentitiesResponse = ListIdentitiesResponse
    loadToMemory = return
