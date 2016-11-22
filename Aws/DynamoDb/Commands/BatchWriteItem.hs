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
-------------------------------------------------------------------------------


data Request = PutRequest { prItem :: Item }
             | DeleteRequest {drKey :: PrimaryKey}
     deriving (Eq,Show,Read,Ord)

data RequestItem = RequestItem{
         reqTable  :: T.Text
       , reqItems  :: [Request]
     } deriving (Eq,Show,Read,Ord)

data BatchWriteItem = BatchWriteItem {
      bwRequests :: [RequestItem]
    -- ^ Put or Delete Requests for a specified table
    , bwRetCons :: ReturnConsumption
    , bwRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------

-- | Construct a RequestItem .
batchRequestItem :: T.Text
               -- ^ A Dynamo table name
               -> [Item]
               -- ^ Items to be saved
               -> [PrimaryKey]
               -- ^ Items to be deleted
               -> RequestItem
batchRequestItem tn its keys = RequestItem tn ((map PutRequest its)++(map DeleteRequest keys))

-- | Construct a BatchWriteItem
batchWriteItem :: [RequestItem]
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

instance ToJSON RequestItem where
   toJSON RequestItem{..} =
       object $
         [ reqTable .= reqItems]

instance ToJSON BatchWriteItem where
    toJSON BatchWriteItem{..} =
        object $
          [ "RequestItems" .= bwRequests
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
    

instance FromJSON RequestItem where
    parseJSON p = do
                 l <- listRqItem p
                 case length l of
                   1 -> return $ head l
                   _ -> fail "Unable to parse RequestItem"
       where
         listRqItem p' = map (\(txt,req) -> RequestItem txt req) . HM.toList <$> parseJSON p'

data BatchWriteItemResponse = BatchWriteItemResponse {
      bwUnprocessed    :: Maybe [RequestItem]
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
