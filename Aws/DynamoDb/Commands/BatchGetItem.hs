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
-------------------------------------------------------------------------------


data GetRequestItem = GetRequestItem{
         griTable  :: T.Text
       , griProjExpr :: T.Text
       , griConsistent ::Bool
       , griKeys :: [PrimaryKey]  
     } deriving (Eq,Show,Read,Ord)

data BatchGetItem = BatchGetItem {
      bgRequests :: [GetRequestItem]
    -- ^ Get Requests for a specified table
    , bgRetCons :: ReturnConsumption
    } deriving (Eq,Show,Read,Ord)


data BatchResponse = BatchResponse {
    brTable :: T.Text
  , brItems :: [Item]
  } deriving (Eq,Show,Read,Ord)
-------------------------------------------------------------------------------

-- | Construct a RequestItem .
batchGetRequestItem :: T.Text
               -- ^ A Dynamo table name
               -> T.Text
               -- ^ Projection Expression
               -> Bool
               -- ^ Consistent Read
               -> [PrimaryKey]
               -- ^ Items to be deleted
               -> GetRequestItem
batchGetRequestItem tn expr consistent keys = GetRequestItem tn expr consistent keys

-- | Construct a BatchGetItem
batchGetItem :: [GetRequestItem]
               -> BatchGetItem
batchGetItem reqs = BatchGetItem reqs def


instance ToJSON GetRequestItem where
   toJSON GetRequestItem{..} =
       object $
         [ griTable .= (object $ [ "ProjectionExpression" .= griProjExpr
                                , "ConsistentRead" .= griConsistent
                                , "Keys" .= griKeys])
         ]

instance ToJSON BatchGetItem where
    toJSON BatchGetItem{..} =
        object $
          [ "RequestItems" .= bgRequests
          , "ReturnConsumedCapacity" .= bgRetCons
          ]

instance FromJSON GetRequestItem where
    parseJSON p = do
                 [(table,Object o)] <- HM.toList <$> parseJSON p 
                 (GetRequestItem table) <$> o .: "ProjectionExpression"
                                        <*> o .: "ConsistentRead"
                                        <*> o .: "Keys"

instance FromJSON BatchResponse where
  parseJSON p = do
              l <- listRqItem p
              case length l of
                1 -> return $ head l
                _ -> fail "unable to parse BatchResponse"
       where
         listRqItem p' = map (\(txt,req) -> BatchResponse txt req) . HM.toList <$> parseJSON p'
         
data BatchGetItemResponse = BatchGetItemResponse {
      bgResponses :: [BatchResponse]
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


