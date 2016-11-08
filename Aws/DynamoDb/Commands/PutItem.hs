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
-- Module      :  Aws.DynamoDb.Commands.GetItem
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- Stability   :  experimental
--
-- @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_PutItem.html@
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.PutItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text           as T
import           Prelude
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


data PutItem = PutItem {
      piTable   :: T.Text
    -- ^ Target table
    , piItem    :: Item
    -- ^ An item to Put. Attributes here will replace what maybe under
    -- the key on DDB.
    , piExpect  :: Conditions
    -- ^ (Possible) set of expections for a conditional Put
    , piReturn  :: UpdateReturn
    -- ^ What to return from this query.
    , piRetCons :: ReturnConsumption
    , piRetMet  :: ReturnItemCollectionMetrics
    } deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------
-- | Construct a minimal 'PutItem' request.
putItem :: T.Text
        -- ^ A Dynamo table name
        -> Item
        -- ^ Item to be saved
        -> PutItem
putItem tn it = PutItem tn it def def def def


instance ToJSON PutItem where
    toJSON PutItem{..} =
        object $ expectsJson piExpect ++
          [ "TableName" .= piTable
          , "Item" .= piItem
          , "ReturnValues" .= piReturn
          , "ReturnConsumedCapacity" .= piRetCons
          , "ReturnItemCollectionMetrics" .= piRetMet
          ]



data PutItemResponse = PutItemResponse {
      pirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , pirConsumed :: Maybe ConsumedCapacity
    -- ^ Amount of capacity consumed
    , pirColMet   :: Maybe ItemCollectionMetrics
    -- ^ Collection metrics if they have been requested.
    } deriving (Eq,Show,Read,Ord)



instance Transaction PutItem PutItemResponse


instance SignQuery PutItem where
    type ServiceConfiguration PutItem = DdbConfiguration
    signQuery gi = ddbSignQuery "PutItem" gi


instance FromJSON PutItemResponse where
    parseJSON (Object v) = PutItemResponse
        <$> v .:? "Attributes"
        <*> v .:? "ConsumedCapacity"
        <*> v .:? "ItemCollectionMetrics"
    parseJSON _ = fail "PutItemResponse must be an object."


instance ResponseConsumer r PutItemResponse where
    type ResponseMetadata PutItemResponse = DdbResponse
    responseConsumer _ _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse PutItemResponse where
    type MemoryResponse PutItemResponse = PutItemResponse
    loadToMemory = return








