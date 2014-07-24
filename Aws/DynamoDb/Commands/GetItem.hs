{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynamoDb.Commands.GetItem
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman
-- Stability   :  experimental
--
--
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.GetItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text           as T
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


-- | A GetItem query that fetches a specific object from DDB.
--
-- See: @http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/API_GetItem.html@
data GetItem = GetItem {
      giTableName  :: T.Text
    , giKey        :: PrimaryKey
    , giAttrs      :: Maybe [T.Text]
    , giConsistent :: Bool
    } deriving (Eq,Show,Read,Ord)


-- | Response to a 'GetItem' query.
data GetItemResponse = GetItemResponse {
      girItem     :: Item
    , girConsumed :: Double
    } deriving (Eq,Show,Read,Ord)


instance Transaction GetItem GetItemResponse


instance ToJSON GetItem where
    toJSON GetItem{..} = object $
        maybe [] (return . ("AttributesToGet" .=)) giAttrs ++
        [ "TableName" .= giTableName
        , "Key" .= giKey
        , "ConsistentRead" .= giConsistent
        ]


instance SignQuery GetItem where
    type ServiceConfiguration GetItem = DdbConfiguration
    signQuery gi = ddbSignQuery "GetItem" gi



instance FromJSON GetItemResponse where
    parseJSON (Object v) = GetItemResponse
        <$> v .: "Item"
        <*> v .: "ConsumedCapacityUnits"
    parseJSON _ = fail "GetItemResponse must be an object."


instance ResponseConsumer r GetItemResponse where
    type ResponseMetadata GetItemResponse = DdbResponse
    responseConsumer _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse GetItemResponse where
    type MemoryResponse GetItemResponse = GetItemResponse
    loadToMemory = return
