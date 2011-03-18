{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Aws.SimpleDb.Commands.BatchPutAttributes
where

import           Aws.Signature
import           Aws.SimpleDb.Info
import           Aws.SimpleDb.Model
import           Aws.SimpleDb.Query
import           Aws.SimpleDb.Response
import           Aws.Transaction
import           Aws.Util
import           Control.Applicative
import           Text.XML.Monad
import qualified Data.ByteString.UTF8  as BU

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

instance SignQuery BatchPutAttributes where
    type Info BatchPutAttributes = SdbInfo
    signQuery BatchPutAttributes{..}
        = sdbSignQuery $ 
            [("Action", "BatchPutAttributes")
            , ("DomainName", BU.fromString bpaDomainName)] ++
            queryList (itemQuery $ queryList (attributeQuery setAttributeQuery) "Attribute") "Item" bpaItems

instance SdbFromResponse BatchPutAttributesResponse where
    sdbFromResponse = BatchPutAttributesResponse <$ testElementNameUI "BatchPutAttributesResponse"

instance Transaction BatchPutAttributes (SdbResponse BatchPutAttributesResponse)
