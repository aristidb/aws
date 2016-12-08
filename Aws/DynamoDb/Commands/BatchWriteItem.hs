{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynamoDb.Commands.BatchWriteItem
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Justin Dawson <jtdawso@gmail.com>
-- Stability   :  experimental
--
-- @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_BatchWriteItem.html@
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.BatchWriteItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import           Data.Foldable (asum)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import           Prelude
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
import           Aws.DynamoDb.Commands.PutItem
import           Aws.DynamoDb.Commands.DeleteItem
-------------------------------------------------------------------------------


data Request = PutRequest { prItem :: Item }
             | DeleteRequest {drKey :: PrimaryKey}
     deriving (Eq,Show,Read,Ord)

data BatchWriteItem = BatchWriteItem {
      bwRequests :: [(T.Text,[Request])]
    -- ^ Put or Delete Requests for a specified table
    , bwRetCons :: ReturnConsumption
    , bwRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------

toBatchWrite :: [PutItem]
           -> [DeleteItem]
           -> BatchWriteItem
toBatchWrite ps ds =BatchWriteItem maps def def  
      where
        maps :: [(T.Text,[Request])]
        maps = let pMap = foldl (\acc p -> let key = piTable p
                                             in HM.insert key (PutRequest (piItem p) : (HM.lookupDefault [] key acc)) acc) HM.empty ps 
                   totalMap = foldl (\acc d -> let key = diTable d
                                                 in  HM.insert key (DeleteRequest (diKey d) : (HM.lookupDefault [] key acc)) acc) pMap ds
                 in  HM.toList totalMap
-- | Construct a BatchWriteItem
batchWriteItem :: [(T.Text,[Request])]
               -> BatchWriteItem
batchWriteItem reqs = BatchWriteItem reqs def def


instance ToJSON Request where
   toJSON PutRequest{..} =
       object $
         [ "PutRequest" .= (object $ ["Item" .= prItem])
         ]
   toJSON DeleteRequest{..} =
       object $
         [ "DeleteRequest" .=  (object $ ["Key" .= drKey])
         ]

instance ToJSON BatchWriteItem where
    toJSON BatchWriteItem{..} =
        object $
          [ "RequestItems" .= HM.fromList bwRequests
          , "ReturnConsumedCapacity" .= bwRetCons
          , "ReturnItemCollectionMetrics" .= bwRetMet
          ]

instance FromJSON Request where
    parseJSON = withObject "PutRequest or DeleteRequest" $ \o ->
     
     asum [
           do
             pr <- o .: "PutRequest"
             i  <- pr .: "Item"
             return $ PutRequest i ,
           do
             dr <- o .: "DeleteRequest"
             pk <- dr .: "Key"
             return $ DeleteRequest pk
          ]
    
data BatchWriteItemResponse = BatchWriteItemResponse {
      bwUnprocessed    :: Maybe [(T.Text,Request)]
    -- ^ Unprocessed Requests on failure
    , bwConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    , bwColMet   :: Maybe ItemCollectionMetrics
    -- ^ Collection metrics for tables affected by BatchWriteItem.
    } deriving (Eq,Show,Read,Ord)



instance Transaction BatchWriteItem BatchWriteItemResponse


instance SignQuery BatchWriteItem where
    type ServiceConfiguration BatchWriteItem = DdbConfiguration
    signQuery gi = ddbSignQuery "BatchWriteItem" gi


instance FromJSON BatchWriteItemResponse where
    parseJSON (Object v) = BatchWriteItemResponse
        <$> v .:? "UnprocessedItems"
        <*> v .:? "ConsumedCapacity"
        <*> v .:? "ItemCollectionMetrics"
    parseJSON _ = fail "BatchWriteItemResponse must be an object."


instance ResponseConsumer r BatchWriteItemResponse where
    type ResponseMetadata BatchWriteItemResponse = DdbResponse
    responseConsumer _ _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse BatchWriteItemResponse where
    type MemoryResponse BatchWriteItemResponse = BatchWriteItemResponse
    loadToMemory = return
