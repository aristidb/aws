{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module Aws.SimpleDb.ListDomains
where

import           Aws.Query
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Monad.Compose.Class
import           Text.XML.Monad
import qualified Data.ByteString.UTF8        as BU

data ListDomains
    = ListDomains {
        ldMaxNumberOfDomains :: Maybe Int
      , ldNextToken :: Maybe String
      }
    deriving (Show)

data ListDomainsResponse 
    = ListDomainsResponse {
        ldrDomainNames :: [String]
      , ldrNextToken :: Maybe String
      }
    deriving (Show)

listDomains :: ListDomains
listDomains = ListDomains { ldMaxNumberOfDomains = Nothing, ldNextToken = Nothing }
             
instance AsQuery ListDomains where
    type Info ListDomains = SdbInfo
    asQuery i ListDomains{..} = addQuery [("Action", "ListDomains")]
                                . addQueryMaybe (BU.fromString . show) ("MaxNumberOfDomains", ldMaxNumberOfDomains)
                                . addQueryMaybe BU.fromString ("NextToken", ldNextToken)
                                $ sdbiBaseQuery i

instance SdbFromResponse ListDomainsResponse where
    sdbFromResponse = do
      testElementNameUI "ListDomainsResponse"
      names <- inList strContent <<< findElementsNameUI "DomainName"
      nextToken <- tryMaybe $ strContent <<< findElementNameUI "NextToken"
      return $ ListDomainsResponse names nextToken

instance Transaction ListDomains (SdbResponse ListDomainsResponse)
