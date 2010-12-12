{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Aws.SimpleDb.DomainMetadata
where

import           Aws.Query
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Applicative
import           Control.Monad.Compose.Class
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.XML.Monad
import qualified Control.Failure             as F

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

instance AsQuery DomainMetadata SdbInfo where
    asQuery i DomainMetadata{..} = addQuery [("Action", "DomainMetadata"), ("DomainName", dmDomainName)] (sdbiBaseQuery i)

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

instance (Monad m, F.Failure SdbError m) => Transaction DomainMetadata SdbInfo SdbError m (SdbResponse DomainMetadataResponse)
