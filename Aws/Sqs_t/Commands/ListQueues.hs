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
import Debug.Trace

data ListQueues = ListQueues {
  lqQueueNamePrefix :: Maybe String
}deriving (Show)

data ListQueuesResponse = ListQueuesResponse{
  lqrQueueUrls :: [T.Text]
} deriving (Show)

listQueues = ListQueues(Nothing)

parse :: Cu.Cursor -> ListQueuesResponse
parse el = do
  let queues = Cu.laxElement "ListQueuesResponse" &/ Cu.laxElement "ListQueuesResult" &/ Cu.laxElement "QueueUrl" &/ Cu.content $ el
  ListQueuesResponse { lqrQueueUrls = queues }

instance SqsResponseIteratee ListQueuesResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = trace  (show $ XML.prettyLBS doc) (Cu.fromDocument doc)
                                            
                                            return $ parse cursor                                  


          
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

