{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module Aws.SimpleDb.DomainMetadata
where

import           Aws.Query
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Applicative
import           Control.Monad.Compose.Class
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.XML.Monad
import qualified Data.ByteString.UTF8        as BU

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

instance AsQuery DomainMetadata where
    type Info DomainMetadata = SdbInfo
    asQuery i DomainMetadata{..} = addQuery [("Action", "DomainMetadata"), ("DomainName", BU.fromString dmDomainName)] (sdbiBaseQuery i)

instance SdbFromResponse DomainMetadataResponse where
    sdbFromResponse = do
      testElementNameUI "DomainMetadataResponse"
      dmrTimestamp <- posixSecondsToUTCTime . fromInteger <$> readContent <<< findElementNameUI "Timestamp"
      dmrItemCount <- readContent <<< findElementNameUI "ItemCount"
      dmrAttributeValueCount <- readContent <<< findElementNameUI "AttributeValueCount"
      dmrAttributeNameCount <- readContent <<< findElementNameUI "AttributeNameCount"
      dmrItemNamesSizeBytes <- readContent <<< findElementNameUI "ItemNamesSizeBytes"
      dmrAttributeValuesSizeBytes <- readContent <<< findElementNameUI "AttributeValuesSizeBytes"
      dmrAttributeNamesSizeBytes <- readContent <<< findElementNameUI "AttributeNamesSizeBytes"
      return DomainMetadataResponse{..}

instance Transaction DomainMetadata (SdbResponse DomainMetadataResponse)
