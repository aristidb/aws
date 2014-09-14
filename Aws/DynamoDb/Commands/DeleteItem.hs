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
-- Module      :  Aws.DynamoDb.Commands.DeleteItem
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- Stability   :  experimental
--
-- @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_DeleteItem.html@
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.DeleteItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text           as T
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


data DeleteItem = DeleteItem {
      diTable   :: T.Text
    -- ^ Target table
    , diKey     :: PrimaryKey
    -- ^ The item to delete.
    , diExpect  :: Conditions
    -- ^ (Possible) set of expections for a conditional Put
    , diReturn  :: UpdateReturn
    -- ^ What to return from this query.
    , diRetCons :: ReturnConsumption
    , diRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------
-- | Construct a minimal 'DeleteItem' request.
deleteItem :: T.Text
        -- ^ A Dynamo table name
        -> PrimaryKey
        -- ^ Item to be saved
        -> DeleteItem
deleteItem tn key = DeleteItem tn key def def def def


instance ToJSON DeleteItem where
    toJSON DeleteItem{..} =
        object $ expectsJson diExpect ++
          [ "TableName" .= diTable
          , "Key" .= diKey
          , "ReturnValues" .= diReturn
          , "ReturnConsumedCapacity" .= diRetCons
          , "ReturnItemCollectionMetrics" .= diRetMet
          ]



data DeleteItemResponse = DeleteItemResponse {
      dirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , dirConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    , dirColMet   :: Maybe ItemCollectionMetrics
    -- ^ Collection metrics if they have been requested.
    } deriving (Eq,Show,Read,Ord)



instance Transaction DeleteItem DeleteItemResponse


instance SignQuery DeleteItem where
    type ServiceConfiguration DeleteItem = DdbConfiguration
    signQuery gi = ddbSignQuery "DeleteItem" gi


instance FromJSON DeleteItemResponse where
    parseJSON (Object v) = DeleteItemResponse
        <$> v .:? "Attributes"
        <*> v .:? "ConsumedCapacity"
        <*> v .:? "ItemCollectionMetrics"
    parseJSON _ = fail "DeleteItemResponse must be an object."


instance ResponseConsumer r DeleteItemResponse where
    type ResponseMetadata DeleteItemResponse = DdbResponse
    responseConsumer _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse DeleteItemResponse where
    type MemoryResponse DeleteItemResponse = DeleteItemResponse
    loadToMemory = return








