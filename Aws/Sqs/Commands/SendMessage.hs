{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.SendMessage where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
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
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data SendMessage = SendMessage{
  smMessage :: T.Text,
  smQueueName :: M.QueueName 
}deriving (Show)

data SendMessageResponse = SendMessageResponse{
  smrMD5OfMessageBody :: T.Text,
  smrMessageId :: M.MessageId
} deriving (Show)

instance ResponseIteratee SendMessageResponse where
    type ResponseMetadata SendMessageResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse el = do
          md5 <- force "Missing MD5 Signature" $ el $// Cu.laxElement "MD5OfMessageBody" &/ Cu.content
          mid <- force "Missing Message Id" $ el $// Cu.laxElement "MessageId" &/ Cu.content
          return SendMessageResponse { smrMD5OfMessageBody = md5, smrMessageId = M.MessageId mid }

instance SignQuery SendMessage  where 
    type Info SendMessage  = SqsInfo
    signQuery SendMessage {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just smQueueName,
                                             sqsQuery = [("Action", Just "SendMessage"), 
                                                        ("MessageBody", Just $ TE.encodeUtf8 smMessage )]} 

instance Transaction SendMessage SendMessageResponse



