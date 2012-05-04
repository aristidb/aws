{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, ScopedTypeVariables, FlexibleContexts #-}

module Aws.Sqs.Commands.Message where

import           Aws.Core
import           Aws.Sqs.Core
import           Control.Applicative
import           Data.Maybe
import           Text.XML.Cursor       (($/), ($//), (&/), (&|))
import qualified Control.Failure       as F
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Text.XML.Cursor       as Cu

data SendMessage = SendMessage{
  smMessage :: T.Text,
  smQueueName :: QueueName
}deriving (Show)

data SendMessageResponse = SendMessageResponse{
  smrMD5OfMessageBody :: T.Text,
  smrMessageId :: MessageId
} deriving (Show)

instance ResponseConsumer r SendMessageResponse where
    type ResponseMetadata SendMessageResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse el = do
          md5 <- force "Missing MD5 Signature" $ el $// Cu.laxElement "MD5OfMessageBody" &/ Cu.content
          mid <- force "Missing Message Id" $ el $// Cu.laxElement "MessageId" &/ Cu.content
          return SendMessageResponse { smrMD5OfMessageBody = md5, smrMessageId = MessageId mid }

instance SignQuery SendMessage  where
    type Info SendMessage  = SqsInfo
    signQuery SendMessage {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just smQueueName,
                                             sqsQuery = [("Action", Just "SendMessage"),
                                                        ("MessageBody", Just $ TE.encodeUtf8 smMessage )]}

instance Transaction SendMessage SendMessageResponse

data DeleteMessage = DeleteMessage{
  dmReceiptHandle :: ReceiptHandle,
  dmQueueName :: QueueName 
}deriving (Show)

data DeleteMessageResponse = DeleteMessageResponse{
} deriving (Show)

instance ResponseConsumer r DeleteMessageResponse where
    type ResponseMetadata DeleteMessageResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse _ = do return DeleteMessageResponse {}
          
instance SignQuery DeleteMessage  where 
    type Info DeleteMessage  = SqsInfo
    signQuery DeleteMessage {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dmQueueName, 
                                             sqsQuery = [("Action", Just "DeleteMessage"), 
                                                        ("ReceiptHandle", Just $ TE.encodeUtf8 $ printReceiptHandle dmReceiptHandle )]} 

instance Transaction DeleteMessage DeleteMessageResponse

data ReceiveMessage
    = ReceiveMessage {
        rmVisibilityTimeout :: Maybe Int
      , rmAttributes :: [MessageAttribute]
      , rmMaxNumberOfMessages :: Maybe Int
      , rmQueueName :: QueueName
      }
    deriving (Show)

data Message
    = Message {
        mMessageId :: T.Text
      , mReceiptHandle :: ReceiptHandle
      , mMD5OfBody :: T.Text
      , mBody :: T.Text
      , mAttributes :: [(MessageAttribute,T.Text)]
      }
    deriving(Show)

data ReceiveMessageResponse
    = ReceiveMessageResponse {
        rmrMessages :: [Message]
      }
    deriving (Show)

readMessageAttribute :: F.Failure XmlException m => Cu.Cursor -> m (MessageAttribute,T.Text)
readMessageAttribute cursor = do
  name <- force "Missing Name" $ cursor $/ Cu.laxElement "Name" &/ Cu.content
  value <- force "Missing Value" $ cursor $/ Cu.laxElement "Value" &/ Cu.content
  parsedName <- parseMessageAttribute name
  return (parsedName, value)

readMessage :: Cu.Cursor -> [Message]
readMessage cursor = do
  mid :: T.Text <- force "Missing Message Id" $ cursor $// Cu.laxElement "MessageId" &/ Cu.content
  rh <- force "Missing Reciept Handle" $ cursor $// Cu.laxElement "ReceiptHandle" &/ Cu.content
  md5 <- force "Missing MD5 Signature" $ cursor $// Cu.laxElement "MD5OfBody" &/ Cu.content
  body <- force "Missing Body" $ cursor $// Cu.laxElement "Body" &/ Cu.content
  let attributes :: [(MessageAttribute, T.Text)] = concat $ cursor $// Cu.laxElement "Attribute" &| readMessageAttribute

  return Message{ mMessageId = mid, mReceiptHandle = ReceiptHandle rh, mMD5OfBody = md5, mBody = body, mAttributes = attributes}

formatMAttributes :: [MessageAttribute] -> [(B.ByteString, Maybe B.ByteString)]
formatMAttributes attrs =
  case length attrs of
    0 -> []
    1 -> [("AttributeName", Just $ B.pack $ show $ attrs !! 0)]
    _ -> zipWith (\ x y -> ((B.concat ["AttributeName.", B.pack $ show $ y]), Just $ TE.encodeUtf8 $ printMessageAttribute x) ) attrs [1 :: Integer ..]

instance ResponseConsumer r ReceiveMessageResponse where
    type ResponseMetadata ReceiveMessageResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where
        parse el = do
          let messages = concat $ el $// Cu.laxElement "Message" &| readMessage
          return ReceiveMessageResponse{ rmrMessages = messages }

instance SignQuery ReceiveMessage  where
    type Info ReceiveMessage  = SqsInfo
    signQuery ReceiveMessage {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just rmQueueName,
                                             sqsQuery = [("Action", Just "ReceiveMessage")] ++
                                                         catMaybes[("VisibilityTimeout",) <$> case rmVisibilityTimeout of
                                                                                                Just x -> Just $ Just $ B.pack $ show x
                                                                                                Nothing -> Nothing,
                                                                   ("MaxNumberOfMessages",) <$> case rmMaxNumberOfMessages of
                                                                                                  Just x -> Just $ Just $ B.pack $ show x
                                                                                                  Nothing -> Nothing] ++ formatMAttributes rmAttributes}

instance Transaction ReceiveMessage ReceiveMessageResponse

data ChangeMessageVisibility = ChangeMessageVisibility {
  cmvReceiptHandle :: ReceiptHandle,
  cmvVisibilityTimeout :: Int,
  cmvQueueName :: QueueName
}deriving (Show)

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse{
} deriving (Show)

instance ResponseConsumer r ChangeMessageVisibilityResponse where
    type ResponseMetadata ChangeMessageVisibilityResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where 
        parse _ = do return ChangeMessageVisibilityResponse{}
    
instance SignQuery ChangeMessageVisibility  where 
    type Info ChangeMessageVisibility  = SqsInfo
    signQuery ChangeMessageVisibility {..} = sqsSignQuery SqsQuery { 
                                             sqsQueueName = Just cmvQueueName, 
                                             sqsQuery = [("Action", Just "ChangeMessageVisibility"), 
                                                         ("ReceiptHandle", Just $ TE.encodeUtf8 $ printReceiptHandle cmvReceiptHandle),
                                                         ("VisibilityTimeout", Just $ B.pack $ show cmvVisibilityTimeout)]}

instance Transaction ChangeMessageVisibility ChangeMessageVisibilityResponse
