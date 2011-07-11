{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.DeleteQueue where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction

data DeleteQueue = DeleteQueue{
  dqQueueName :: M.QueueName 
}deriving (Show)

data DeleteQueueResponse = DeleteQueueResponse{
} deriving (Show)

instance ResponseIteratee r DeleteQueueResponse where
    type ResponseMetadata DeleteQueueResponse = SqsMetadata
    responseIteratee _ = sqsXmlResponseIteratee parse
      where
        parse _ = do return DeleteQueueResponse{}
          
instance SignQuery DeleteQueue  where 
    type Info DeleteQueue  = SqsInfo
    signQuery DeleteQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dqQueueName, 
                                             sqsQuery = [("Action", Just "DeleteQueue")]}

instance Transaction DeleteQueue DeleteQueueResponse
