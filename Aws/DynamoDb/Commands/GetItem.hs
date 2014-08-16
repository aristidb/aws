{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynamoDb.Commands.GetItem
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- Stability   :  experimental
--
--
----------------------------------------------------------------------------

module Aws.DynamoDb.Commands.GetItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
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
    -- ^ Attributes to get. 'Nothing' grabs everything.
    , giConsistent :: Bool
    -- ^ Whether to issue a consistent read.
    , giRetCons    :: ReturnConsumption
    -- ^ Whether to return consumption stats.
    } deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------
-- | Construct a minimal 'GetItem' request.
getItem
    :: T.Text                   -- ^ Table name
    -> PrimaryKey               -- ^ Primary key
    -> GetItem
getItem tn k = GetItem tn k Nothing False def


-- | Response to a 'GetItem' query.
data GetItemResponse = GetItemResponse {
      girItem     :: Maybe Item
    , girConsumed :: Maybe ConsumedCapacity
    } deriving (Eq,Show,Read,Ord)


instance Transaction GetItem GetItemResponse


instance ToJSON GetItem where
    toJSON GetItem{..} = object $
        maybe [] (return . ("AttributesToGet" .=)) giAttrs ++
        [ "TableName" .= giTableName
        , "Key" .= giKey
        , "ConsistentRead" .= giConsistent
        , "ReturnConsumedCapacity" .= giRetCons
        ]


instance SignQuery GetItem where
    type ServiceConfiguration GetItem = DdbConfiguration
    signQuery gi = ddbSignQuery "GetItem" gi



instance FromJSON GetItemResponse where
    parseJSON (Object v) = GetItemResponse
        <$> v .:? "Item"
        <*> v .:? "ConsumedCapacity"
    parseJSON _ = fail "GetItemResponse must be an object."


instance ResponseConsumer r GetItemResponse where
    type ResponseMetadata GetItemResponse = DdbResponse
    responseConsumer _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse GetItemResponse where
    type MemoryResponse GetItemResponse = GetItemResponse
    loadToMemory = return
