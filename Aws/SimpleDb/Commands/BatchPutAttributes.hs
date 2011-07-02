{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.BatchPutAttributes
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

data BatchPutAttributes
    = BatchPutAttributes {
        bpaItems :: [Item [Attribute SetAttribute]]
      , bpaDomainName :: T.Text
      }
    deriving (Show)

data BatchPutAttributesResponse
    = BatchPutAttributesResponse
    deriving (Show)
             
batchPutAttributes :: [Item [Attribute SetAttribute]] -> T.Text -> BatchPutAttributes
batchPutAttributes items domain = BatchPutAttributes { bpaItems = items, bpaDomainName = domain }

instance SignQuery BatchPutAttributes where
    type Info BatchPutAttributes = SdbInfo
    signQuery BatchPutAttributes{..}
        = sdbSignQuery $ 
            [("Action", "BatchPutAttributes")
            , ("DomainName", T.encodeUtf8 bpaDomainName)] ++
            queryList (itemQuery $ queryList (attributeQuery setAttributeQuery) "Attribute") "Item" bpaItems

instance ResponseIteratee BatchPutAttributesResponse where
    type ResponseMetadata BatchPutAttributesResponse = SdbMetadata
    responseIteratee = sdbResponseIteratee $ sdbCheckResponseType BatchPutAttributesResponse "BatchPutAttributesResponse"

instance Transaction BatchPutAttributes BatchPutAttributesResponse
