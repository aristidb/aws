{-# LANGUAGE OverloadedStrings #-}
module Aws.Sqs.Model where

import Data.Text
import Debug.Trace

data QueueAttribute =
  All
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

parseAttribute :: Text -> QueueAttribute
parseAttribute "ApproximateNumberOfMessages" = ApproximateNumberOfMessages 
parseAttribute "ApproximateNumberOfMessagesNotVisible" = ApproximateNumberOfMessagesNotVisible
parseAttribute "VisibilityTimeout" = VisibilityTimeout
parseAttribute "CreatedTimestamp" = CreatedTimestamp
parseAttribute "LastModifiedTimestamp" = LastModifiedTimestamp
parseAttribute "Policy" = Policy
parseAttribute "MaximumMessageSize" = MaximumMessageSize
parseAttribute "MessageRetentionPeriod" = MessageRetentionPeriod
parseAttribute "QueueArn" = QueueArn
parseAttribute x = trace(show x)(error $ unpack x) 

newtype RecieptHandle = RecieptHandle Text deriving(Show,Eq)
newtype MessageId = MessageId Text deriving(Show,Eq)
