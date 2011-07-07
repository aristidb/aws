{-# LANGUAGE OverloadedStrings #-}
module Aws.Sqs.Model where

import Data.Maybe
import qualified Data.Text as T
import Debug.Trace

data QueueName = QueueName{
  qName :: T.Text,
  qAccountNumber :: T.Text
} deriving(Show)


parseQueueUrl :: T.Text -> QueueName
parseQueueUrl url = QueueName{qAccountNumber = urlParts !! 3, qName = urlParts !! 4}
  where
    urlParts = T.splitOn "/" url
 

printQueueName :: QueueName -> T.Text
printQueueName queue = T.concat ["/", (qAccountNumber queue), "/", (qName queue), "/"]

data QueueAttribute =
  QueueAll
  | ApproximateNumberOfMessages
  | ApproximateNumberOfMessagesNotVisible
  | VisibilityTimeout
  | CreatedTimestamp
  | LastModifiedTimestamp
  | Policy
  | MaximumMessageSize
  | MessageRetentionPeriod
  | QueueArn
  deriving(Show, Enum, Eq)

data MessageAttribute = 
  MessageAll
  | SenderId
  | SentTimestamp
  | ApproximateReceiveCount
  | ApproximateFirstReceiveTimestamp
  deriving(Show,Eq,Enum)

data SqsPermission =
  PermissionAll
  | SendMessage
  | RecieveMessage
  | DeleteMessage
  | ChangeMessageVisibility
  | GetQueueAttributes
  deriving (Show, Enum, Eq)

parseQueueAttribute :: T.Text -> QueueAttribute
parseQueueAttribute "ApproximateNumberOfMessages" = ApproximateNumberOfMessages 
parseQueueAttribute "ApproximateNumberOfMessagesNotVisible" = ApproximateNumberOfMessagesNotVisible
parseQueueAttribute "VisibilityTimeout" = VisibilityTimeout
parseQueueAttribute "CreatedTimestamp" = CreatedTimestamp
parseQueueAttribute "LastModifiedTimestamp" = LastModifiedTimestamp
parseQueueAttribute "Policy" = Policy
parseQueueAttribute "MaximumMessageSize" = MaximumMessageSize
parseQueueAttribute "MessageRetentionPeriod" = MessageRetentionPeriod
parseQueueAttribute "QueueArn" = QueueArn
parseQueueAttribute x = trace(show x)(error $ T.unpack x) 

printQueueAttribute :: QueueAttribute -> T.Text
printQueueAttribute QueueAll = "All"
printQueueAttribute ApproximateNumberOfMessages = "ApproximateNumberOfMessages"
printQueueAttribute ApproximateNumberOfMessagesNotVisible = "ApproximateNumberOfMessagesNotVisible"
printQueueAttribute VisibilityTimeout = "VisibilityTimeout"
printQueueAttribute CreatedTimestamp = "CreatedTimestamp"
printQueueAttribute LastModifiedTimestamp = "LastModifiedTimestamp"
printQueueAttribute Policy = "Policy"
printQueueAttribute MaximumMessageSize = "MaximumMessageSize"
printQueueAttribute MessageRetentionPeriod = "MessageRetentionPeriod"
printQueueAttribute QueueArn = "QueueArn"

parseMessageAttribute :: T.Text -> MessageAttribute
parseMessageAttribute "SenderId" = SenderId
parseMessageAttribute "SentTimestamp" = SentTimestamp
parseMessageAttribute "ApproximateReceiveCount" = ApproximateReceiveCount
parseMessageAttribute "ApproximateFirstReceiveTimestamp" = ApproximateFirstReceiveTimestamp

printMessageAttribute :: MessageAttribute -> T.Text
printMessageAttribute MessageAll = "All"
printMessageAttribute SenderId = "SenderId"
printMessageAttribute SentTimestamp = "SentTimestamp"
printMessageAttribute ApproximateReceiveCount = "ApproximateReceiveCount"
printMessageAttribute ApproximateFirstReceiveTimestamp = "ApproximateFirstRecieveTimestamp"

printPermission PermissionAll = "*"
printPermission SendMessage = "SendMessage"
printPermission RecieveMessage = "ReceiveMessage"
printPermission DeleteMessage = "DeleteMessage"
printPermission ChangeMessageVisibility = "ChangeMessageVisibility"
printPermission GetQueueAttributes = "GetQueueAttributes"

newtype ReceiptHandle = ReceiptHandle T.Text deriving(Show,Eq)
newtype MessageId = MessageId T.Text deriving(Show,Eq)
