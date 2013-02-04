{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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
    , piItem   :: Item
    , piExpect :: Maybe PutExpect
    , piReturn :: PutReturn
    } deriving (Eq,Show,Read,Ord)


putItem = PutItem { piTable = ""
                  , piItem = defItem
                  , piExpect = Nothing
                  , piReturn = RNone }

instance ToJSON PutItem where
    toJSON PutItem{..} = object $
        maybe [] (return . ("Expected" .=)) piExpect ++
        [ "TableName" .= piTable
        , "Item" .= piItem
        , "ReturnValues" .= piReturn
        ]



data PutReturn = RNone | RAll
    deriving (Eq,Show,Read,Ord)


instance ToJSON PutReturn where
    toJSON RNone = toJSON ("NONE" :: T.Text)
    toJSON RAll = toJSON ("ALL_OLD" :: T.Text)


-- | Perform 'PutItem' only if 'peExists' matches the reality for the
-- other parameters here.
data PutExpect = PutExpect {
      peAttr   :: T.Text
    -- ^ Attribute for the existence check
    , peVal    :: Maybe DValue
    -- ^ Further constrain this check and make it apply only if
    -- attribute has this value
    , peExists :: Bool
    -- ^ If 'True', will only match if attribute exists. If 'False'
    -- will only match if the attribute is missing.
    } deriving (Eq,Show,Read,Ord)


instance ToJSON PutExpect where
    toJSON PutExpect{..} = object [ peAttr .= object sub ]
        where
          sub = maybe [] (return . ("Value" .= )) peVal ++
                ["Exists" .= peExists]


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








