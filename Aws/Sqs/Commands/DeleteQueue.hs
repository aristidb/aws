{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.DeleteQueue where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response

data DeleteQueue = DeleteQueue{
  dqQueueName :: M.QueueName 
}deriving (Show)

data DeleteQueueResponse = DeleteQueueResponse{
} deriving (Show)

instance ResponseConsumer r DeleteQueueResponse where
    type ResponseMetadata DeleteQueueResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse _ = do return DeleteQueueResponse{}
          
instance SignQuery DeleteQueue  where 
    type Info DeleteQueue  = SqsInfo
    signQuery DeleteQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dqQueueName, 
                                             sqsQuery = [("Action", Just "DeleteQueue")]}

instance Transaction DeleteQueue DeleteQueueResponse
