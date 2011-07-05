{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.CreateQueue where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
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

data CreateQueue = CreateQueue{
  cqDefaultVisibilityTimeout :: Maybe Int,
  cqQueueName :: T.Text
}deriving (Show)

data CreateQueueResponse = CreateQueueResponse{
  cqrQueueUrl :: T.Text
} deriving (Show)


instance ResponseIteratee CreateQueueResponse where
    type ResponseMetadata CreateQueueResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse el = do 
          let url = T.concat $ concat $ force "Missing Queue Url" $ el $// Cu.laxElement "QueueUrl" &| Cu.content
          return CreateQueueResponse{ cqrQueueUrl = url}
          
instance SignQuery CreateQueue  where 
    type Info CreateQueue  = SqsInfo
    signQuery CreateQueue {..} = sqsSignQuery SqsQuery { 
                                             sqsQuery = [("Action", Just "CreateQueue"), 
                                                        ("QueueName", Just $ B.pack $ T.unpack cqQueueName)] ++ 
                                                        catMaybes [("DefaultVisibilityTimeout",) <$> case cqDefaultVisibilityTimeout of
                                                                                                       Just x -> Just $ Just $ B.pack $ show x
                                                                                                       Nothing -> Nothing]}

instance Transaction CreateQueue CreateQueueResponse
