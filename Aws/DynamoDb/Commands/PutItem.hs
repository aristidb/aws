{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Aws.DynamoDb.Commands.PutItem where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import qualified Data.Text           as T
-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.DynamoDb.Core
-------------------------------------------------------------------------------


data PutItem = PutItem {
      piTable  :: T.Text
    -- ^ Target table
    , piItem   :: Item
    -- ^ An item to Put
    , piExpect :: [Expect]
    -- ^ (Possible) set of expections for a conditional Put
    , piReturn :: PutReturn
    -- ^ What to return from this query.
    } deriving (Eq,Show,Read,Ord)



-- | A simple starting point for a 'PutItem' request.
--
-- It sets 'piExpect' to 'Nothing' and 'piReturn' to 'RNone'.
putItem :: T.Text
        -- ^ A Dynamo table name
        -> Item
        -- ^ Item to be saved
        -> PutItem
putItem tn it = PutItem tn it [] RNone


instance ToJSON PutItem where
    toJSON PutItem{..} =
        let exp = if (piExpect == [])
                    then []
                    else ["Expected" .= piExpect]
        in object $ exp ++
          [ "TableName" .= piTable
          , "Item" .= piItem
          , "ReturnValues" .= piReturn
          ]



data PutReturn = RNone | RAll
    deriving (Eq,Show,Read,Ord)


instance ToJSON PutReturn where
    toJSON RNone = toJSON ("NONE" :: T.Text)
    toJSON RAll = toJSON ("ALL_OLD" :: T.Text)



data PutItemResponse = PutItemResponse {
      pirAttrs    :: Maybe Item
    -- ^ Old attributes, if requested
    , pirConsumed :: Double
    -- ^ Amount of capacity consumed
    } deriving (Eq,Show,Read,Ord)



instance Transaction PutItem PutItemResponse


instance SignQuery PutItem where
    type ServiceConfiguration PutItem = DdbConfiguration
    signQuery gi = ddbSignQuery gi "PutItem"


instance FromJSON PutItemResponse where
    parseJSON (Object v) = PutItemResponse
        <$> v .:? "Attributes"
        <*> v .: "ConsumedCapacityUnits"


instance ResponseConsumer r PutItemResponse where
    type ResponseMetadata PutItemResponse = DdbResponse
    responseConsumer rq ref resp = ddbResponseConsumer ref resp


instance AsMemoryResponse PutItemResponse where
    type MemoryResponse PutItemResponse = PutItemResponse
    loadToMemory = return








