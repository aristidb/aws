{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.CreateDomain
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import qualified Data.ByteString.UTF8  as BU

data CreateDomain
    = CreateDomain {
        cdDomainName :: String
      }
    deriving (Show)

data CreateDomainResponse 
    = CreateDomainResponse
    deriving (Show)
             
createDomain :: String -> CreateDomain
createDomain name = CreateDomain { cdDomainName = name }
             
instance SignQuery CreateDomain where
    type Info CreateDomain = SdbInfo
    signQuery CreateDomain{..} = sdbSignQuery [("Action", "CreateDomain"), ("DomainName", BU.fromString cdDomainName)]

instance ResponseIteratee CreateDomainResponse where
    type ResponseMetadata CreateDomainResponse = SdbMetadata
    responseIteratee = sdbResponseIteratee $ sdbCheckResponseType CreateDomainResponse "CreateDomainResponse"

instance Transaction CreateDomain CreateDomainResponse
