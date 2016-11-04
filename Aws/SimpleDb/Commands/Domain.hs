module Aws.SimpleDb.Commands.Domain where

import           Aws.Core
import           Aws.SimpleDb.Core
import           Control.Applicative
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.XML.Cursor       (($//), (&|))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

data CreateDomain
    = CreateDomain {
        cdDomainName :: T.Text
      }
    deriving (Show)

data CreateDomainResponse 
    = CreateDomainResponse
    deriving (Show)
             
createDomain :: T.Text -> CreateDomain
createDomain name = CreateDomain { cdDomainName = name }
             
-- | ServiceConfiguration: 'SdbConfiguration'
instance SignQuery CreateDomain where
    type ServiceConfiguration CreateDomain = SdbConfiguration
    signQuery CreateDomain{..} = sdbSignQuery [("Action", "CreateDomain"), ("DomainName", T.encodeUtf8 cdDomainName)]

instance ResponseConsumer r CreateDomainResponse where
    type ResponseMetadata CreateDomainResponse = SdbMetadata
    responseConsumer _ _
        = sdbResponseConsumer $ sdbCheckResponseType CreateDomainResponse "CreateDomainResponse"

instance Transaction CreateDomain CreateDomainResponse

instance AsMemoryResponse CreateDomainResponse where
    type MemoryResponse CreateDomainResponse = CreateDomainResponse
    loadToMemory = return

data DeleteDomain
    = DeleteDomain {
        ddDomainName :: T.Text
      }
    deriving (Show)

data DeleteDomainResponse
    = DeleteDomainResponse
    deriving (Show)
             
deleteDomain :: T.Text -> DeleteDomain
deleteDomain name = DeleteDomain { ddDomainName = name }
             
-- | ServiceConfiguration: 'SdbConfiguration'
instance SignQuery DeleteDomain where
    type ServiceConfiguration DeleteDomain = SdbConfiguration
    signQuery DeleteDomain{..} = sdbSignQuery [("Action", "DeleteDomain"), ("DomainName", T.encodeUtf8 ddDomainName)]

instance ResponseConsumer r DeleteDomainResponse where
    type ResponseMetadata DeleteDomainResponse = SdbMetadata
    responseConsumer _ _
        = sdbResponseConsumer $ sdbCheckResponseType DeleteDomainResponse "DeleteDomainResponse"

instance Transaction DeleteDomain DeleteDomainResponse

instance AsMemoryResponse DeleteDomainResponse where
    type MemoryResponse DeleteDomainResponse = DeleteDomainResponse
    loadToMemory = return

data DomainMetadata
    = DomainMetadata {
        dmDomainName :: T.Text
      }
    deriving (Show)

data DomainMetadataResponse
    = DomainMetadataResponse {
        dmrTimestamp :: UTCTime
      , dmrItemCount :: Integer
      , dmrAttributeValueCount :: Integer
      , dmrAttributeNameCount :: Integer
      , dmrItemNamesSizeBytes :: Integer
      , dmrAttributeValuesSizeBytes :: Integer
      , dmrAttributeNamesSizeBytes :: Integer
      }
    deriving (Show)

domainMetadata :: T.Text -> DomainMetadata
domainMetadata name = DomainMetadata { dmDomainName = name }

-- | ServiceConfiguration: 'SdbConfiguration'
instance SignQuery DomainMetadata where
    type ServiceConfiguration DomainMetadata = SdbConfiguration
    signQuery DomainMetadata{..} = sdbSignQuery [("Action", "DomainMetadata"), ("DomainName", T.encodeUtf8 dmDomainName)]

instance ResponseConsumer r DomainMetadataResponse where
    type ResponseMetadata DomainMetadataResponse = SdbMetadata

    responseConsumer _ _
        = sdbResponseConsumer parse
        where parse cursor = do
                sdbCheckResponseType () "DomainMetadataResponse" cursor
                dmrTimestamp <- forceM "Timestamp expected" $ cursor $// elCont "Timestamp" &| (fmap posixSecondsToUTCTime . readInt)
                dmrItemCount <- forceM "ItemCount expected" $ cursor $// elCont "ItemCount" &| readInt
                dmrAttributeValueCount <- forceM "AttributeValueCount expected" $ cursor $// elCont "AttributeValueCount" &| readInt
                dmrAttributeNameCount <- forceM "AttributeNameCount expected" $ cursor $// elCont "AttributeNameCount" &| readInt
                dmrItemNamesSizeBytes <- forceM "ItemNamesSizeBytes expected" $ cursor $// elCont "ItemNamesSizeBytes" &| readInt
                dmrAttributeValuesSizeBytes <- forceM "AttributeValuesSizeBytes expected" $ cursor $// elCont "AttributeValuesSizeBytes" &| readInt
                dmrAttributeNamesSizeBytes <- forceM "AttributeNamesSizeBytes expected" $ cursor $// elCont "AttributeNamesSizeBytes" &| readInt
                return DomainMetadataResponse{..}

instance Transaction DomainMetadata DomainMetadataResponse

instance AsMemoryResponse DomainMetadataResponse where
    type MemoryResponse DomainMetadataResponse = DomainMetadataResponse
    loadToMemory = return

data ListDomains
    = ListDomains {
        ldMaxNumberOfDomains :: Maybe Int
      , ldNextToken :: Maybe T.Text
      }
    deriving (Show)

data ListDomainsResponse
    = ListDomainsResponse {
        ldrDomainNames :: [T.Text]
      , ldrNextToken :: Maybe T.Text
      }
    deriving (Show)

listDomains :: ListDomains
listDomains = ListDomains { ldMaxNumberOfDomains = Nothing, ldNextToken = Nothing }

-- | ServiceConfiguration: 'SdbConfiguration'
instance SignQuery ListDomains where
    type ServiceConfiguration ListDomains = SdbConfiguration
    signQuery ListDomains{..} = sdbSignQuery $ catMaybes [
                                  Just ("Action", "ListDomains")
                                , ("MaxNumberOfDomains",) . T.encodeUtf8 . T.pack . show <$> ldMaxNumberOfDomains
                                , ("NextToken",) . T.encodeUtf8 <$> ldNextToken
                                ]

instance ResponseConsumer r ListDomainsResponse where
    type ResponseMetadata ListDomainsResponse = SdbMetadata
    responseConsumer _ _ = sdbResponseConsumer parse
        where parse cursor = do
                sdbCheckResponseType () "ListDomainsResponse" cursor
                let names = cursor $// elContent "DomainName"
                let nextToken = listToMaybe $ cursor $// elContent "NextToken"
                return $ ListDomainsResponse names nextToken

instance Transaction ListDomains ListDomainsResponse

instance AsMemoryResponse ListDomainsResponse where
    type MemoryResponse ListDomainsResponse = ListDomainsResponse
    loadToMemory = return

instance ListResponse ListDomainsResponse T.Text where
    listResponse = ldrDomainNames

instance IteratedTransaction ListDomains ListDomainsResponse where
  nextIteratedRequest req ListDomainsResponse{ldrNextToken=nt} = req{ldNextToken=nt} <$ nt
  --combineIteratedResponse (ListDomainsResponse dn1 _) (ListDomainsResponse dn2 nt2) = ListDomainsResponse (dn1 ++ dn2) nt2
