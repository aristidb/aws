{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.SimpleDb.Commands.ListDomains
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Enumerator.Cursor (($//))
import qualified Data.ByteString.UTF8       as BU

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
    sdbFromResponse cursor = do
      sdbCheckResponseType () "ListDomainsResponse" cursor
      let names = cursor $// elCont "DomainName"
      let nextToken = listToMaybe $ cursor $// elCont "NextToken"
      return $ ListDomainsResponse names nextToken

instance Transaction ListDomains (SdbResponse ListDomainsResponse)
