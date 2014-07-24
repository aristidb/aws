{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Aws.DynamoDb.Commands.UpdateItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import qualified Data.Text           as T
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


-- | An @UpdateItem@ request.
data UpdateItem = UpdateItem {
      uiTable   :: T.Text
    , uiKey     :: PrimaryKey
    , uiUpdates :: [AttributeUpdate]
    , uiExpect  :: [Expect]
    , uiReturn  :: UpdateReturn
    } deriving (Eq,Show,Read,Ord)


type AttributeUpdates = [AttributeUpdate]


data AttributeUpdate = AttributeUpdate {
      auName   :: T.Text
    -- ^ Attribute name
    , auValue  :: DValue
    -- ^ Attribute value
    , auAction :: UpdateAction
    -- ^ Type of update operation.
    } deriving (Eq,Show,Read,Ord)


instance ToJSON AttributeUpdates where
    toJSON = object . map mk
        where
          mk AttributeUpdate{..} = auName .= object ["Value" .= auValue, "Action" .= auAction]


data UpdateAction = UPut | UAdd | UDelete
    deriving (Eq,Show,Read,Ord)


instance ToJSON UpdateAction where
    toJSON UPut = toJSON ("PUT" :: T.Text)
    toJSON UAdd = toJSON ("ADD" :: T.Text)
    toJSON UDelete = toJSON ("DELETE" :: T.Text)


instance Default UpdateAction where
    def = UPut


data UpdateReturn = URNone | URAllOld | URUpdatedOld | URAllNew | URUpdatedNew
    deriving (Eq,Show,Read,Ord)


instance Default UpdateReturn where
    def = URNone


instance ToJSON UpdateReturn where
    toJSON URNone = toJSON ("NONE" :: T.Text)
    toJSON URAllOld = toJSON ("ALL_OLD" :: T.Text)
    toJSON URUpdatedOld = toJSON ("UPDATED_OLD" :: T.Text)
    toJSON URAllNew = toJSON ("ALL_NEW" :: T.Text)
    toJSON URUpdatedNew = toJSON ("UPDATED_NEW" :: T.Text)


instance ToJSON UpdateItem where
    toJSON UpdateItem{..} =
        let expect = if (uiExpect == [])
                     then []
                     else ["Expected" .= uiExpect]
        in object $ expect ++
          [ "TableName" .= uiTable
          , "Key" .= uiKey
          , "AttributeUpdates" .= uiUpdates
          , "ReturnValues" .= uiReturn
          ]


data UpdateItemResponse = UpdateItemResponse {
      uirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , uirConsumed :: Double
    -- ^ Amount of capacity consumed
    } deriving (Eq,Show,Read,Ord)



instance Transaction UpdateItem UpdateItemResponse


instance SignQuery UpdateItem where
    type ServiceConfiguration UpdateItem = DdbConfiguration
    signQuery gi = ddbSignQuery "UpdateItem" gi


instance FromJSON UpdateItemResponse where
    parseJSON (Object v) = UpdateItemResponse
        <$> v .:? "Attributes"
        <*> v .: "ConsumedCapacityUnits"
    parseJSON _ = fail "UpdateItemResponse expected a JSON object"


instance ResponseConsumer r UpdateItemResponse where
    type ResponseMetadata UpdateItemResponse = DdbResponse
    responseConsumer _ ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse UpdateItemResponse where
    type MemoryResponse UpdateItemResponse = UpdateItemResponse
    loadToMemory = return








