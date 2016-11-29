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
         griTable  :: T.Text
       , griProjExpr :: Maybe T.Text
       , griConsistent ::Bool
       , griKeys :: [PrimaryKey]  
     } deriving (Eq,Show,Read,Ord)

data BatchGetItem = BatchGetItem {
      bgRequests :: [GetRequestItem]
    -- ^ Get Requests for a specified table
    , bgRetCons :: ReturnConsumption
    } deriving (Eq,Show,Read,Ord)


data BatchGetResponse = BatchGetResponse {
    brTable :: T.Text
  , brItems :: [Item]
  } deriving (Eq,Show,Read,Ord)
-------------------------------------------------------------------------------

-- | Construct a RequestItem .
batchGetRequestItem :: T.Text
               -- ^ A Dynamo table name
               -> Maybe T.Text
               -- ^ Projection Expression
               -> Bool
               -- ^ Consistent Read
               -> [PrimaryKey]
               -- ^ Items to be deleted
               -> GetRequestItem
batchGetRequestItem tn expr consistent keys = GetRequestItem tn expr consistent keys

toBatchGet :: [GetItem] -> BatchGetItem
toBatchGet gs = BatchGetItem (convert gs) def

  where
    groupItems :: [GetItem]-> HM.HashMap T.Text [GetItem] -> HM.HashMap T.Text [GetItem]
    groupItems [] hm = hm
    groupItems (x:xs) hm = let key = giTableName x
                             in groupItems xs (HM.insert key (x : (HM.lookupDefault [] key hm)) hm)
    
    convert :: [GetItem] -> [GetRequestItem] 
    convert gs' = let l = HM.toList $ groupItems gs' HM.empty
                    -- Uses one GetItem to specify ProjectionExpression
                    -- and ConsistentRead for the entire batch
                    in map (\(table,items@(i:_)) ->GetRequestItem table
                                                    (T.intercalate "," <$> giAttrs i)
                                                    (giConsistent i)
                                                    (map giKey items) ) l
-- | Construct a BatchGetItem
batchGetItem :: [GetRequestItem]
               -> BatchGetItem
batchGetItem reqs = BatchGetItem reqs def


instance ToJSON GetRequestItem where
   toJSON GetRequestItem{..} =
       object $
         [ griTable .= (object $ maybe [] (return . ("ProjectionExpression" .=)) griProjExpr ++
                                ["ConsistentRead" .= griConsistent
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
                 (GetRequestItem table) <$> o .:? "ProjectionExpression"
                                        <*> o .: "ConsistentRead"
                                        <*> o .: "Keys"

instance FromJSON BatchGetResponse where
  parseJSON p = do
              l <- listRqItem p
              case length l of
                1 -> return $ head l
                _ -> fail "unable to parse BatchGetResponse"
       where
         listRqItem p' = map (\(txt,req) -> BatchGetResponse txt req) . HM.toList <$> parseJSON p'
         
data BatchGetItemResponse = BatchGetItemResponse {
      bgResponses :: [BatchGetResponse]
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


