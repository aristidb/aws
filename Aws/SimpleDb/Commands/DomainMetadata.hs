{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.DomainMetadata
where

import           Aws.Signature
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Xml
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.XML.Enumerator.Cursor (($//), (&|))
import qualified Data.ByteString.UTF8       as BU

data DomainMetadata
    = DomainMetadata {
        dmDomainName :: String
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
             
domainMetadata :: String -> DomainMetadata
domainMetadata name = DomainMetadata { dmDomainName = name }

instance SignQuery DomainMetadata where
    type Info DomainMetadata = SdbInfo
    signQuery DomainMetadata{..} = sdbSignQuery [("Action", "DomainMetadata"), ("DomainName", BU.fromString dmDomainName)]

instance SdbFromResponse DomainMetadataResponse where
    sdbFromResponse cursor = do
      sdbCheckResponseType () "DomainMetadataResponse" cursor
      dmrTimestamp <- sdbForceM "Timestamp expected" $ cursor $// elCont "Timestamp" &| (fmap posixSecondsToUTCTime . sdbReadInt)
      dmrItemCount <- sdbForceM "ItemCount expected" $ cursor $// elCont "ItemCount" &| sdbReadInt
      dmrAttributeValueCount <- sdbForceM "AttributeValueCount expected" $ cursor $// elCont "AttributeValueCount" &| sdbReadInt
      dmrAttributeNameCount <- sdbForceM "AttributeNameCount expected" $ cursor $// elCont "AttributeNameCount" &| sdbReadInt
      dmrItemNamesSizeBytes <- sdbForceM "ItemNamesSizeBytes expected" $ cursor $// elCont "ItemNamesSizeBytes" &| sdbReadInt
      dmrAttributeValuesSizeBytes <- sdbForceM "AttributeValuesSizeBytes expected" $ cursor $// elCont "AttributeValuesSizeBytes" &| sdbReadInt
      dmrAttributeNamesSizeBytes <- sdbForceM "AttributeNamesSizeBytes expected" $ cursor $// elCont "AttributeNamesSizeBytes" &| sdbReadInt
      return DomainMetadataResponse{..}

instance Transaction DomainMetadata (SdbResponse DomainMetadataResponse)
