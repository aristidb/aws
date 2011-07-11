{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.DomainMetadata
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Xml
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.XML.Enumerator.Cursor (($//), (&|))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

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

instance SignQuery DomainMetadata where
    type Info DomainMetadata = SdbInfo
    signQuery DomainMetadata{..} = sdbSignQuery [("Action", "DomainMetadata"), ("DomainName", T.encodeUtf8 dmDomainName)]

instance ResponseIteratee r DomainMetadataResponse where
    type ResponseMetadata DomainMetadataResponse = SdbMetadata

    responseIteratee _ = sdbResponseIteratee parse
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
