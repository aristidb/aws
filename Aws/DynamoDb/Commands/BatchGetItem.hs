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
-- Module      :  Aws.DynamoDb.Commands.BatchGetItem
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Justin Dawson <jtdawso@gmail.com>
-- Stability   :  experimental
--
-- @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_BatchGetItem.html@
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.BatchGetItem where

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
import           Aws.DynamoDb.Commands.GetItem
-------------------------------------------------------------------------------


data GetRequestItem = GetRequestItem{
         griProjExpr :: Maybe T.Text
       , griConsistent ::Bool
       , griKeys :: [PrimaryKey]  
     } deriving (Eq,Show,Read,Ord)

data BatchGetItem = BatchGetItem {
      bgRequests :: [(T.Text,GetRequestItem)]
    -- ^ Get Requests for a specified table
    , bgRetCons :: ReturnConsumption
    } deriving (Eq,Show,Read,Ord)

-------------------------------------------------------------------------------

-- | Construct a RequestItem .
batchGetRequestItem :: Maybe T.Text
               -- ^ Projection Expression
               -> Bool
               -- ^ Consistent Read
               -> [PrimaryKey]
               -- ^ Items to be deleted
               -> GetRequestItem
batchGetRequestItem expr consistent keys = GetRequestItem expr consistent keys

toBatchGet :: [GetItem] -> BatchGetItem
toBatchGet gs = BatchGetItem (convert gs) def

  where
    groupItems :: [GetItem]-> HM.HashMap T.Text [GetItem] -> HM.HashMap T.Text [GetItem]
    groupItems [] hm = hm
    groupItems (x:xs) hm = let key = giTableName x
                             in groupItems xs (HM.insert key (x : (HM.lookupDefault [] key hm)) hm)
    
    convert :: [GetItem] -> [(T.Text,GetRequestItem)] 
    convert gs' = let l = HM.toList $ groupItems gs' HM.empty
                    -- Uses one GetItem to specify ProjectionExpression
                    -- and ConsistentRead for the entire batch
                    in map (\(table,items@(i:_)) ->(table,GetRequestItem 
                                                    (T.intercalate "," <$> giAttrs i)
                                                    (giConsistent i)
                                                    (map giKey items)) ) l

-- | Construct a BatchGetItem
batchGetItem :: [(T.Text,GetRequestItem)]
               -> BatchGetItem
batchGetItem reqs = BatchGetItem reqs def


instance ToJSON GetRequestItem where
   toJSON GetRequestItem{..} =
       (object $ maybe [] (return . ("ProjectionExpression" .=)) griProjExpr ++
                 ["ConsistentRead" .= griConsistent
                 , "Keys" .= griKeys])
         

instance ToJSON BatchGetItem where
    toJSON BatchGetItem{..} =
        object $
          [ "RequestItems" .= HM.fromList bgRequests
          , "ReturnConsumedCapacity" .= bgRetCons
          ]

instance FromJSON GetRequestItem where
    parseJSON (Object p) = do
                 GetRequestItem <$> p .:? "ProjectionExpression"
                                <*> p .: "ConsistentRead"
                                <*> p .: "Keys"
    parseJSON _ = fail "unable to parse GetRequestItem"
    
         
data BatchGetItemResponse = BatchGetItemResponse {
      bgResponses :: [(T.Text, [Item])]
    , bgUnprocessed    :: Maybe [GetRequestItem]
    -- ^ Unprocessed Requests on failure
    , bgConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    } deriving (Eq,Show,Read,Ord)



instance Transaction BatchGetItem BatchGetItemResponse


instance SignQuery BatchGetItem where
    type ServiceConfiguration BatchGetItem = DdbConfiguration
    signQuery gi = ddbSignQuery "BatchGetItem" gi


instance FromJSON BatchGetItemResponse where
    parseJSON (Object v) = BatchGetItemResponse
        <$> v .: "Responses"
        <*> v .:? "UnprocessedItems"
        <*> v .:? "ConsumedCapacity"

    parseJSON _ = fail "BatchGetItemResponse must be an object."

instance ResponseConsumer r BatchGetItemResponse where
    type ResponseMetadata BatchGetItemResponse = DdbResponse
    responseConsumer _ _ ref resp = ddbResponseConsumer ref resp

instance AsMemoryResponse BatchGetItemResponse where
    type MemoryResponse BatchGetItemResponse = BatchGetItemResponse
    loadToMemory = return


