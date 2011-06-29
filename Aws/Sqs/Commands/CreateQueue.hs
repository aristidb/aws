{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ChangeMessageVisibility where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Model
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
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|), ($|))
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data CreateQueue = CreateQueue
  cqVisibilityTimeout :: Maybe Int,
  cqQueueName :: String
}deriving (Show)

data CreateQueueResponse = CreateQueueResponse{
  cqrQueueUrl :: Text
} deriving (Show)


cqParse :: Cu.Cursor -> CreateQueueResponse
cqParse el = do
  let url = head $ Cu.laxElement "CreateQueueResponse" &/ Cu.laxElement "CreateQueueResult" $/ Cu.laxElement "QueueUrl" $| Cu.content $ el
  CreateQueueResponse(cqrQueueUrl = url)

instance SqsResponseIteratee ChangeMessageVisibilityResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = Cu.fromDocument doc
                                            return $ cqParse cursor                                  
          
instance SignQuery ChangeMessageVisibility  where 
    type Info ChangeMessageVisibility  = SqsInfo
    signQuery ChangeMessageVisibility {..} = sqsSignQuery SqsQuery { 
                                             sqsQuery = [("Action", Just "CreateQueue"), 
                                                         ("QueueName", Just $ B.pack cqQueueName),
                                                         ("VisibilityTimout", Just $ B.pack $ show cmvVisibilityTimeout)]}

instance Transaction ChangeMessageVisibility (SqsResponse ChangeMessageVisibilityResponse)
