{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.DeleteMessage where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model                as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import qualified Data.Text.Encoding           as TE

data DeleteMessage = DeleteMessage{
  dmReceiptHandle :: M.ReceiptHandle,
  dmQueueName :: M.QueueName 
}deriving (Show)

data DeleteMessageResponse = DeleteMessageResponse{
} deriving (Show)

instance ResponseConsumer r DeleteMessageResponse where
    type ResponseMetadata DeleteMessageResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse _ = do return DeleteMessageResponse {}
          
instance SignQuery DeleteMessage  where 
    type Info DeleteMessage  = SqsInfo
    signQuery DeleteMessage {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dmQueueName, 
                                             sqsQuery = [("Action", Just "DeleteMessage"), 
                                                        ("ReceiptHandle", Just $ TE.encodeUtf8 $ M.printReceiptHandle dmReceiptHandle )]} 

instance Transaction DeleteMessage DeleteMessageResponse


