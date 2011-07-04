{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.BatchDeleteAttributes
where

import           Aws.Response
import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Metadata
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T

data BatchDeleteAttributes
    = BatchDeleteAttributes {
        bdaItems :: [Item [Attribute DeleteAttribute]]
      , bdaDomainName :: T.Text
      }
    deriving (Show)

data BatchDeleteAttributesResponse
    = BatchDeleteAttributesResponse
    deriving (Show)
             
batchDeleteAttributes :: [Item [Attribute DeleteAttribute]] -> T.Text -> BatchDeleteAttributes
batchDeleteAttributes items domain = BatchDeleteAttributes { bdaItems = items, bdaDomainName = domain }

instance SignQuery BatchDeleteAttributes where
    type Info BatchDeleteAttributes = SdbInfo
    signQuery BatchDeleteAttributes{..}
        = sdbSignQuery $ 
            [("Action", "BatchDeleteAttributes")
            , ("DomainName", T.encodeUtf8 bdaDomainName)] ++
            queryList (itemQuery $ queryList (attributeQuery deleteAttributeQuery) "Attribute") "Item" bdaItems

instance ResponseIteratee BatchDeleteAttributesResponse where
    type ResponseMetadata BatchDeleteAttributesResponse = SdbMetadata
    responseIteratee = sdbResponseIteratee $ sdbCheckResponseType BatchDeleteAttributesResponse "BatchDeleteAttributesResponse"

instance Transaction BatchDeleteAttributes BatchDeleteAttributesResponse
