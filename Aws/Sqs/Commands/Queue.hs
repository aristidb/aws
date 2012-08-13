
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

-- | ServiceConfiguration: 'SqsConfiguration'
instance SignQuery CreateQueue  where
    type ServiceConfiguration CreateQueue  = SqsConfiguration
    signQuery CreateQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Nothing,
                                             sqsQuery = [("Action", Just "CreateQueue"),
                                                        ("QueueName", Just $ TE.encodeUtf8 cqQueueName)] ++
                                                        catMaybes [("DefaultVisibilityTimeout",) <$> case cqDefaultVisibilityTimeout of
                                                                                                       Just x -> Just $ Just $ B.pack $ show x
                                                                                                       Nothing -> Nothing]}

instance Transaction CreateQueue CreateQueueResponse

instance AsMemoryResponse CreateQueueResponse where
    type MemoryResponse CreateQueueResponse = CreateQueueResponse
    loadToMemory = return

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
          
-- | ServiceConfiguration: 'SqsConfiguration'
instance SignQuery DeleteQueue  where 
    type ServiceConfiguration DeleteQueue  = SqsConfiguration
    signQuery DeleteQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dqQueueName, 
                                             sqsQuery = [("Action", Just "DeleteQueue")]}

instance Transaction DeleteQueue DeleteQueueResponse

instance AsMemoryResponse DeleteQueueResponse where
    type MemoryResponse DeleteQueueResponse = DeleteQueueResponse
    loadToMemory = return

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

-- | ServiceConfiguration: 'SqsConfiguration'
instance SignQuery ListQueues where
    type ServiceConfiguration ListQueues = SqsConfiguration
    signQuery ListQueues{..} = sqsSignQuery SqsQuery {
                                              sqsQueueName = Nothing,
                                              sqsQuery = [("Action", Just "ListQueues")] ++ catMaybes [
                                              ("QueueNamePrefix",) <$> case lqQueueNamePrefix of
                                                                         Just x  -> Just $ Just $ TE.encodeUtf8 x
                                                                         Nothing -> Nothing]}

instance Transaction ListQueues ListQueuesResponse

instance AsMemoryResponse ListQueuesResponse where
    type MemoryResponse ListQueuesResponse = ListQueuesResponse
    loadToMemory = return