{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.SendMessage where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Text.XML.Enumerator.Cursor   (($//), (&/))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Enumerator.Cursor   as Cu

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



