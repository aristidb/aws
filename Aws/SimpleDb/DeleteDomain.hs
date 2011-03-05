{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.DeleteDomain
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Applicative
import           Text.XML.Monad
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

instance SdbFromResponse DeleteDomainResponse where
    sdbFromResponse = DeleteDomainResponse <$ testElementNameUI "DeleteDomainResponse"

instance Transaction DeleteDomain (SdbResponse DeleteDomainResponse)
