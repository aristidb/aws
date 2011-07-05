{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ListQueues where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import           Aws.Sqs.Query
import           Aws.Sqs.Response
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
  lqQueueNamePrefix :: Maybe T.Text
}deriving (Show)

data ListQueuesResponse = ListQueuesResponse{
  lqrQueueUrls :: [T.Text]
} deriving (Show)

listQueues = ListQueues(Nothing)

instance ResponseIteratee ListQueuesResponse where
    type ResponseMetadata ListQueuesResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where
        parse el = do 
            let queues = concat $ sequence $ el $// Cu.laxElement "QueueUrl" &| Cu.content
            return ListQueuesResponse { lqrQueueUrls = queues }
          
instance SignQuery ListQueues where 
    type Info ListQueues = SqsInfo
    signQuery ListQueues{..} = sqsSignQuery SqsQuery { 
                                              sqsQueueName = Nothing, 
                                              sqsQuery = HTTP.simpleQueryToQuery $ map (second BU.fromString) $ catMaybes [
                                              ("Action",) <$> Just("ListQueues") ,
                                              ("QueueNamePrefix",) <$> case lqQueueNamePrefix of
                                                                         Just x  -> Just $ T.unpack x
                                                                         Nothing -> Nothing]}

instance Transaction ListQueues ListQueuesResponse

