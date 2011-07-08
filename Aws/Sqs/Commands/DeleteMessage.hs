{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.DeleteMessage where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model                as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Enumerator.Cursor   as Cu
import Debug.Trace

data DeleteMessage = DeleteMessage{
  dmReceiptHandle :: M.ReceiptHandle,
  dmQueueName :: M.QueueName 
}deriving (Show)

data DeleteMessageResponse = DeleteMessageResponse{
} deriving (Show)

instance ResponseIteratee DeleteMessageResponse where
    type ResponseMetadata DeleteMessageResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where
        parse el = do return DeleteMessageResponse {}
          
instance SignQuery DeleteMessage  where 
    type Info DeleteMessage  = SqsInfo
    signQuery DeleteMessage {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dmQueueName, 
                                             sqsQuery = [("Action", Just "DeleteMessage"), 
                                                        ("ReceiptHandle", Just $ TE.encodeUtf8 $ M.printReceiptHandle dmReceiptHandle )]} 

instance Transaction DeleteMessage DeleteMessageResponse


