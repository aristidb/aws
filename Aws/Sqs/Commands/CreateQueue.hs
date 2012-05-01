{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.CreateQueue where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Cursor              (($//), (&/))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Cursor              as Cu
import qualified Data.ByteString.Char8        as B

data CreateQueue = CreateQueue{
  cqDefaultVisibilityTimeout :: Maybe Int,
  cqQueueName :: T.Text
}deriving (Show)

data CreateQueueResponse = CreateQueueResponse{
  cqrQueueUrl :: T.Text
} deriving (Show)


instance ResponseConsumer r CreateQueueResponse where
    type ResponseMetadata CreateQueueResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse el = do
          url <- force "Missing Queue Url" $ el $// Cu.laxElement "QueueUrl" &/ Cu.content
          return CreateQueueResponse{ cqrQueueUrl = url}

instance SignQuery CreateQueue  where
    type Info CreateQueue  = SqsInfo
    signQuery CreateQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Nothing,
                                             sqsQuery = [("Action", Just "CreateQueue"),
                                                        ("QueueName", Just $ TE.encodeUtf8 cqQueueName)] ++
                                                        catMaybes [("DefaultVisibilityTimeout",) <$> case cqDefaultVisibilityTimeout of
                                                                                                       Just x -> Just $ Just $ B.pack $ show x
                                                                                                       Nothing -> Nothing]}

instance Transaction CreateQueue CreateQueueResponse
