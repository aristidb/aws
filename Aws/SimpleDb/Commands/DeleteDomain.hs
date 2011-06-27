{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.DeleteDomain
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import qualified Data.ByteString.UTF8  as BU

data DeleteDomain
    = DeleteDomain {
        ddDomainName :: String
      }
    deriving (Show)

data DeleteDomainResponse
    = DeleteDomainResponse
    deriving (Show)
             
deleteDomain :: String -> DeleteDomain
deleteDomain name = DeleteDomain { ddDomainName = name }
             
instance SignQuery DeleteDomain where
    type Info DeleteDomain = SdbInfo
    signQuery DeleteDomain{..} = sdbSignQuery [("Action", "DeleteDomain"), ("DomainName", BU.fromString ddDomainName)]

instance ResponseIteratee DeleteDomainResponse where
    type ResponseMetadata DeleteDomainResponse = SdbMetadata
    responseIteratee = sdbResponseIteratee $ sdbCheckResponseType DeleteDomainResponse "DeleteDomainResponse"

instance Transaction DeleteDomain DeleteDomainResponse
