{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.BatchDeleteAttributes
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import qualified Data.ByteString.UTF8       as BU

data BatchDeleteAttributes
    = BatchDeleteAttributes {
        bdaItems :: [Item [Attribute DeleteAttribute]]
      , bdaDomainName :: String
      }
    deriving (Show)

data BatchDeleteAttributesResponse
    = BatchDeleteAttributesResponse
    deriving (Show)
             
batchDeleteAttributes :: [Item [Attribute DeleteAttribute]] -> String -> BatchDeleteAttributes
batchDeleteAttributes items domain = BatchDeleteAttributes { bdaItems = items, bdaDomainName = domain }

instance SignQuery BatchDeleteAttributes where
    type Info BatchDeleteAttributes = SdbInfo
    signQuery BatchDeleteAttributes{..}
        = sdbSignQuery $ 
            [("Action", "BatchDeleteAttributes")
            , ("DomainName", BU.fromString bdaDomainName)] ++
            queryList (itemQuery $ queryList (attributeQuery deleteAttributeQuery) "Attribute") "Item" bdaItems

instance SdbFromResponse BatchDeleteAttributesResponse where
    sdbFromResponse = sdbCheckResponseType BatchDeleteAttributesResponse "BatchDeleteAttributesResponse"

instance Transaction BatchDeleteAttributes (SdbResponse BatchDeleteAttributesResponse)
