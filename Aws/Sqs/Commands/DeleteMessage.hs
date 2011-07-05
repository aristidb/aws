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
import qualified Network.HTTP.Types           as HTTP
import qualified Data.ByteString.UTF8         as BU
import qualified Data.ByteString.Char8        as B
import Debug.Trace

data DeleteMessage = DeleteMessage{
  dmReceiptHandle :: M.ReceiptHandle,
  dmQueueName :: M.QueueName 
}deriving (Show)

data DeleteMessageResponse = DeleteMessageResponse{
} deriving (Show)


dmParse :: Cu.Cursor -> DeleteMessageResponse
dmParse el = do
  DeleteMessageResponse { }

instance ResponseIteratee DeleteMessageResponse where
    type ResponseMetadata DeleteMessageResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where
        parse el = do return DeleteMessageResponse {}
          
instance SignQuery DeleteMessage  where 
    type Info DeleteMessage  = SqsInfo
    signQuery DeleteMessage {..} = sqsSignQuery SqsQuery { 
                                             sqsQuery = [("Action", Just "DeleteMessage"), 
                                                        ("QueueName", Just $ B.pack $ T.unpack $ M.printQueueName dmQueueName),
                                                        ("ReceiptHandle", Just $ B.pack $ show dmReceiptHandle )]} 

instance Transaction DeleteMessage DeleteMessageResponse


