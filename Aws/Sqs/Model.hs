{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Aws.Sqs.Model where

import           Aws.Xml
import qualified Control.Failure as F
import qualified Data.Text       as T

data QueueName = QueueName{
  qName :: T.Text,
  qAccountNumber :: T.Text
} deriving(Show)

printQueueName :: QueueName -> T.Text
printQueueName queue = T.concat ["/", (qAccountNumber queue), "/", (qName queue), "/"]

data QueueAttribute
    = QueueAll
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

data MessageAttribute
    = MessageAll
    | SenderId
    | SentTimestamp
    | ApproximateReceiveCount
    | ApproximateFirstReceiveTimestamp
    deriving(Show,Eq,Enum)

data SqsPermission
    = PermissionAll
    | SendMessage
    | ReceiveMessage
    | DeleteMessage
    | ChangeMessageVisibility
    | GetQueueAttributes
    deriving (Show, Enum, Eq)

parseQueueAttribute :: F.Failure XmlException m  => T.Text -> m QueueAttribute
parseQueueAttribute "ApproximateNumberOfMessages" = return ApproximateNumberOfMessages 
parseQueueAttribute "ApproximateNumberOfMessagesNotVisible" = return ApproximateNumberOfMessagesNotVisible
parseQueueAttribute "VisibilityTimeout" = return VisibilityTimeout
parseQueueAttribute "CreatedTimestamp" = return CreatedTimestamp
parseQueueAttribute "LastModifiedTimestamp" = return LastModifiedTimestamp
parseQueueAttribute "Policy" = return Policy
parseQueueAttribute "MaximumMessageSize" = return MaximumMessageSize
parseQueueAttribute "MessageRetentionPeriod" = return MessageRetentionPeriod
parseQueueAttribute "QueueArn" = return QueueArn
parseQueueAttribute x = F.failure $ XmlException ( "Invalid Attribute Name. " ++ show x)

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

parseMessageAttribute :: F.Failure XmlException m  =>  T.Text -> m MessageAttribute
parseMessageAttribute "SenderId" = return SenderId
parseMessageAttribute "SentTimestamp" = return SentTimestamp
parseMessageAttribute "ApproximateReceiveCount" = return ApproximateReceiveCount
parseMessageAttribute "ApproximateFirstReceiveTimestamp" = return ApproximateFirstReceiveTimestamp
parseMessageAttribute x = F.failure $ XmlException ( "Invalid Attribute Name. " ++ show x)

printMessageAttribute :: MessageAttribute -> T.Text
printMessageAttribute MessageAll = "All"
printMessageAttribute SenderId = "SenderId"
printMessageAttribute SentTimestamp = "SentTimestamp"
printMessageAttribute ApproximateReceiveCount = "ApproximateReceiveCount"
printMessageAttribute ApproximateFirstReceiveTimestamp = "ApproximateFirstReceiveTimestamp"

printPermission :: SqsPermission -> T.Text
printPermission PermissionAll = "*"
printPermission SendMessage = "SendMessage"
printPermission ReceiveMessage = "ReceiveMessage"
printPermission DeleteMessage = "DeleteMessage"
printPermission ChangeMessageVisibility = "ChangeMessageVisibility"
printPermission GetQueueAttributes = "GetQueueAttributes"

newtype ReceiptHandle = ReceiptHandle T.Text deriving(Show,Eq)
newtype MessageId = MessageId T.Text deriving(Show,Eq)

printReceiptHandle :: ReceiptHandle -> T.Text
printReceiptHandle (ReceiptHandle handle) = handle 
