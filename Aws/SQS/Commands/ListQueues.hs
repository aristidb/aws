{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.SQS.Commands.ListQueues where

import           Aws.Response
import           Aws.SQS.Error
import           Aws.SQS.Info
import           Aws.SQS.Query
import           Aws.SQS.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Control.Arrow         (second)
import           Control.Monad
import           Data.Enumerator              ((=$))
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|))
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU

data ListQueues = ListQueues {
  lqQueueNamePrefix :: Maybe String
}deriving (Show)

data ListQueuesResponse = ListQueuesResponse{
  lqrQueueUrls :: [String]
} deriving (Show)

listQueues = ListQueues(Nothing)

instance SqsResponseIteratee ListQueuesResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = Cu.fromDocument doc
                                            case parse cursor of                                  
                                              Left err -> En.throwError err
                                              Right v -> return v
        where
          parse :: Cu.Cursor -> Either SqsError ListQueuesResponse
          parse el = do
            queues <- sequence $ el $// Cu.laxElement "ListQueuesResult" &| parseQueue
            return ListQueuesResponse { lqrQueueUrls = queues }
          
          parseQueue :: Cu.Cursor -> Either SqsError String
          parseQueue el = do
            queueUrl <- sqsForce "Missing queue Name" $ el $/ elCont "QueueUrl"
            return queueUrl


instance SignQuery ListQueues where 
    type Info ListQueues = SqsInfo
    signQuery ListQueues{..} = sqsSignQuery SqsQuery { 
                                              sqsQueueName = Nothing, 
                                              sqsQuery = HTTP.simpleQueryToQuery $ map (second BU.fromString) $ catMaybes [
                                              ("Action",) <$> Just("ListQueues") ,
                                              ("QueueNamePrefix",) <$> lqQueueNamePrefix
                                            ]
 }

instance Transaction ListQueues (SqsResponse ListQueuesResponse)

