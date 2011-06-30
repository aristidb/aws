{-# LANGUAGE OverloadedStrings #-}
module Aws.Sqs.Model where

import Data.Maybe
import qualified Data.Text as T
import Debug.Trace
import Text.Regex

data QueueName = QueueName{
  qName :: String,
  qAccountNumber :: String
} deriving(Show)

urlRegex = mkRegex "/([0-9]+)/([a-zA-Z0-9]+)/"

parseQueue :: String -> QueueName
parseQueue url = QueueName{qAccountNumber = validMatches !! 0, qName = validMatches !! 1}
  where
    matches = matchRegex urlRegex url
    validMatches = case matches of
                    Just(x) -> x
                    Nothing -> []

printQueue queue = "/" ++ (qAccountNumber queue) ++ "/" ++ (qName queue) ++ "/"

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

parseAttribute :: T.Text -> QueueAttribute
parseAttribute "ApproximateNumberOfMessages" = ApproximateNumberOfMessages 
parseAttribute "ApproximateNumberOfMessagesNotVisible" = ApproximateNumberOfMessagesNotVisible
parseAttribute "VisibilityTimeout" = VisibilityTimeout
parseAttribute "CreatedTimestamp" = CreatedTimestamp
parseAttribute "LastModifiedTimestamp" = LastModifiedTimestamp
parseAttribute "Policy" = Policy
parseAttribute "MaximumMessageSize" = MaximumMessageSize
parseAttribute "MessageRetentionPeriod" = MessageRetentionPeriod
parseAttribute "QueueArn" = QueueArn
parseAttribute x = trace(show x)(error $ T.unpack x) 

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

parseMessageAttribute "SenderId" = SenderId
parseMessageAttribute "SentTimestamp" = SentTimestamp
parseMessageAttribute "ApproximateReceiveCount" = ApproximateReceiveCount
parseMessageAttribute "ApproximateFirstReceiveTimestamp" = ApproximateFirstReceiveTimestamp

printMessageAttribute MessageAll = "All"
printMessageAttribute SenderId = "SenderId"
printMessageAttribute SentTimestamp = "SentTimestamp"
printMessageAttribute ApproximateReceiveCount = "ApproximateReceiveCount"
printMessageAttribute ApproximateFirstReceiveTimestamp = "ApproximateFirstRecieveTimestamp"

newtype RecieptHandle = RecieptHandle T.Text deriving(Show,Eq)
newtype MessageId = MessageId T.Text deriving(Show,Eq)
