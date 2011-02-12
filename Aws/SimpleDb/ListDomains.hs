{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeFamilies, OverloadedStrings, TupleSections #-}

module Aws.SimpleDb.ListDomains
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Applicative
import           Control.Monad.Compose.Class
import           Data.Maybe
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
             
instance SignQuery ListDomains where
    type Info ListDomains = SdbInfo
    signQuery ListDomains{..} = sdbSignQuery $ catMaybes [
                                  Just ("Action", "ListDomains")
                                , ("MaxNumberOfDomains",) <$> BU.fromString <$> show <$> ldMaxNumberOfDomains
                                , ("NextToken",) <$> BU.fromString <$> ldNextToken
                                ]

instance SdbFromResponse ListDomainsResponse where
    sdbFromResponse = do
      testElementNameUI "ListDomainsResponse"
      names <- inList strContent <<< findElementsNameUI "DomainName"
      nextToken <- tryMaybe $ strContent <<< findElementNameUI "NextToken"
      return $ ListDomainsResponse names nextToken

instance Transaction ListDomains (SdbResponse ListDomainsResponse)
