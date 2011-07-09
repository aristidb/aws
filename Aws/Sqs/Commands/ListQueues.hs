{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ListQueues where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Enumerator.Cursor   (($//), (&/))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Enumerator.Cursor   as Cu

data ListQueues = ListQueues {
  lqQueueNamePrefix :: Maybe T.Text
}deriving (Show)

data ListQueuesResponse = ListQueuesResponse{
  lqrQueueUrls :: [T.Text]
} deriving (Show)

instance ResponseIteratee ListQueuesResponse where
    type ResponseMetadata ListQueuesResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
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

