{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.Queue where

import           Aws.Core
import           Aws.Sqs.Core
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Cursor       (($//), (&/))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Text.XML.Cursor       as Cu
import qualified Data.ByteString.Char8 as B

data CreateQueue = CreateQueue {
    cqDefaultVisibilityTimeout :: Maybe Int,
    cqQueueName :: T.Text
  } deriving (Show)

data CreateQueueResponse = CreateQueueResponse {
    cqrQueueUrl :: T.Text
  } deriving (Show)


instance ResponseConsumer r CreateQueueResponse where
    type ResponseMetadata CreateQueueResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse el = do
          url <- force "Missing Queue Url" $ el $// Cu.laxElement "QueueUrl" &/ Cu.content
          return CreateQueueResponse{ cqrQueueUrl = url}

instance SignQuery CreateQueue  where
    type Info CreateQueue  = SqsInfo
    signQuery CreateQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Nothing,
                                             sqsQuery = [("Action", Just "CreateQueue"),
                                                        ("QueueName", Just $ TE.encodeUtf8 cqQueueName)] ++
                                                        catMaybes [("DefaultVisibilityTimeout",) <$> case cqDefaultVisibilityTimeout of
                                                                                                       Just x -> Just $ Just $ B.pack $ show x
                                                                                                       Nothing -> Nothing]}

instance Transaction CreateQueue CreateQueueResponse

data DeleteQueue = DeleteQueue {
    dqQueueName :: QueueName 
  } deriving (Show)

data DeleteQueueResponse = DeleteQueueResponse 
  deriving (Show)

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

data ListQueues = ListQueues {
    lqQueueNamePrefix :: Maybe T.Text
  } deriving (Show)

data ListQueuesResponse = ListQueuesResponse {
    lqrQueueUrls :: [T.Text]
  } deriving (Show)

instance ResponseConsumer r ListQueuesResponse where
    type ResponseMetadata ListQueuesResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse el = do
            let queues = el $// Cu.laxElement "QueueUrl" &/ Cu.content
            return ListQueuesResponse { lqrQueueUrls = queues }

instance SignQuery ListQueues where
    type Info ListQueues = SqsInfo
    signQuery ListQueues{..} = sqsSignQuery SqsQuery {
                                              sqsQueueName = Nothing,
                                              sqsQuery = [("Action", Just "ListQueues")] ++ catMaybes [
                                              ("QueueNamePrefix",) <$> case lqQueueNamePrefix of
                                                                         Just x  -> Just $ Just $ TE.encodeUtf8 x
                                                                         Nothing -> Nothing]}

instance Transaction ListQueues ListQueuesResponse
