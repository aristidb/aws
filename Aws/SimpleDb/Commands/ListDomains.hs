{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
module Aws.SimpleDb.Commands.ListDomains
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Enumerator.Cursor (($//))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T

data ListDomains
    = ListDomains {
        ldMaxNumberOfDomains :: Maybe Int
      , ldNextToken :: Maybe T.Text
      }
    deriving (Show)

data ListDomainsResponse 
    = ListDomainsResponse {
        ldrDomainNames :: [T.Text]
      , ldrNextToken :: Maybe T.Text
      }
    deriving (Show)

listDomains :: ListDomains
listDomains = ListDomains { ldMaxNumberOfDomains = Nothing, ldNextToken = Nothing }
             
instance SignQuery ListDomains where
    type Info ListDomains = SdbInfo
    signQuery ListDomains{..} = sdbSignQuery $ catMaybes [
                                  Just ("Action", "ListDomains")
                                , ("MaxNumberOfDomains",) . T.encodeUtf8 . T.pack . show <$> ldMaxNumberOfDomains
                                , ("NextToken",) . T.encodeUtf8 <$> ldNextToken
                                ]

instance ResponseIteratee r ListDomainsResponse where
    type ResponseMetadata ListDomainsResponse = SdbMetadata
    responseIteratee _ = sdbResponseIteratee parse 
        where parse cursor = do
                sdbCheckResponseType () "ListDomainsResponse" cursor
                let names = cursor $// elContent "DomainName"
                let nextToken = listToMaybe $ cursor $// elContent "NextToken"
                return $ ListDomainsResponse names nextToken

instance Transaction ListDomains ListDomainsResponse
