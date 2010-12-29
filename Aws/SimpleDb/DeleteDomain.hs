{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies #-}

module Aws.SimpleDb.DeleteDomain
where

import Aws.Query
import Aws.SimpleDb.Info
import Aws.SimpleDb.Response
import Aws.Transaction
import Control.Applicative
import Text.XML.Monad

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
             
instance AsQuery DeleteDomain where
    type Info DeleteDomain = SdbInfo
    asQuery i DeleteDomain{..} = addQuery [("Action", "DeleteDomain"), ("DomainName", ddDomainName)] (sdbiBaseQuery i)

instance SdbFromResponse DeleteDomainResponse where
    sdbFromResponse = DeleteDomainResponse <$ testElementNameUI "DeleteDomainResponse"

instance Transaction DeleteDomain (SdbResponse DeleteDomainResponse)
