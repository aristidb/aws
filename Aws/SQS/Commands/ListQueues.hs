{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ListQueues where

data ListQueues = ListQueues {
  lqQueueNamePrefix :: Maybe String
}deriving (Show)

data ListQueuesResponse = ListQueuesResponse{
  lqrQueueUrls :: [String]
} deriving (Show)

listQueues = ListQueues(Nothing)

instance SignQuery ListQueues = 


