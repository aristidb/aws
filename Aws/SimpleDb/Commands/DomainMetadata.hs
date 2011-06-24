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
      let readNum s = case reads s of
                        [(n,"")] -> Right $ fromInteger n
                        _        -> Left $ SdbXmlError "Integer expected" Nothing
      dmrTimestamp <- sdbForceM "Timestamp expected" $ cursor $// elCont "Timestamp" &| (fmap posixSecondsToUTCTime . readNum)
      dmrItemCount <- sdbForceM "ItemCount expected" $ cursor $// elCont "ItemCount" &| readNum
      dmrAttributeValueCount <- sdbForceM "AttributeValueCount expected" $ cursor $// elCont "AttributeValueCount" &| readNum
      dmrAttributeNameCount <- sdbForceM "AttributeNameCount expected" $ cursor $// elCont "AttributeNameCount" &| readNum
      dmrItemNamesSizeBytes <- sdbForceM "ItemNamesSizeBytes expected" $ cursor $// elCont "ItemNamesSizeBytes" &| readNum
      dmrAttributeValuesSizeBytes <- sdbForceM "AttributeValuesSizeBytes expected" $ cursor $// elCont "AttributeValuesSizeBytes" &| readNum
      dmrAttributeNamesSizeBytes <- sdbForceM "AttributeNamesSizeBytes expected" $ cursor $// elCont "AttributeNamesSizeBytes" &| readNum
      return DomainMetadataResponse{..}

instance Transaction DomainMetadata (SdbResponse DomainMetadataResponse)
