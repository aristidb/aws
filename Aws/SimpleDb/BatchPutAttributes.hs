{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Aws.SimpleDb.BatchPutAttributes
where

import           Aws.Query
import           Aws.SimpleDb.Error
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Control.Applicative
import           Text.XML.Monad
import qualified Control.Failure       as F

data BatchPutAttributes
    = BatchPutAttributes {
        bpaItems :: [Item [Attribute SetAttribute]]
      , bpaDomainName :: String
      }
    deriving (Show)

data BatchPutAttributesResponse
    = BatchPutAttributesResponse
    deriving (Show)
             
batchPutAttributes :: [Item [Attribute SetAttribute]] -> String -> BatchPutAttributes
batchPutAttributes items domain = BatchPutAttributes { bpaItems = items, bpaDomainName = domain }

instance AsQuery BatchPutAttributes SdbInfo where
    asQuery i BatchPutAttributes{..}
        = addQuery [("Action", "BatchPutAttributes"), ("DomainName", bpaDomainName)]
          . addQueryList (itemQuery $ queryList (attributeQuery setAttributeQuery) "Attribute") "Item" bpaItems
          $ sdbiBaseQuery i

instance SdbFromResponse BatchPutAttributesResponse where
    sdbFromResponse = BatchPutAttributesResponse <$ testElementNameUI "BatchPutAttributesResponse"

instance Transaction BatchPutAttributes SdbInfo (SdbResponse BatchPutAttributesResponse)
