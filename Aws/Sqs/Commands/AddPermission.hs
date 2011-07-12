{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.AddPermission where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import           Aws.Sqs.Model
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import qualified Data.Text             as T
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Types    as HTTP


data AddPermission = AddPermission{
  apLabel :: T.Text,
  apPermissions :: [(T.Text,SqsPermission)],
  apQueueName :: QueueName
}deriving (Show)

data AddPermissionResponse = AddPermissionResponse{
} deriving (Show)


formatPermissions :: [(T.Text,SqsPermission)] -> [HTTP.QueryItem]
formatPermissions perms = 
  concat $ zipWith(\ x y -> [(B.pack $ "AwsAccountId." ++ show y, Just $ B.pack $ T.unpack $ fst x), 
                             (B.pack $ "ActionName." ++ show y, Just $ B.pack $ T.unpack $ printPermission $ snd x)]) perms [1 :: Integer ..]

instance ResponseIteratee r AddPermissionResponse where
    type ResponseMetadata AddPermissionResponse = SqsMetadata
    responseIteratee _ = sqsXmlResponseIteratee parse
       where
         parse _ = do
           return AddPermissionResponse {}
        
instance SignQuery AddPermission  where 
    type Info AddPermission  = SqsInfo
    signQuery AddPermission {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just apQueueName, 
                                             sqsQuery = [("Action", Just "AddPermission"), 
                                                        ("QueueName", Just $ B.pack $ T.unpack $ printQueueName apQueueName),
                                                        ("Label", Just $ B.pack $ T.unpack apLabel)] ++ formatPermissions apPermissions}

instance Transaction AddPermission AddPermissionResponse
