{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, FlexibleContexts, ScopedTypeVariables #-}

module Aws.Sqs.Commands.ReceiveMessage where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import           Aws.Xml
import           Control.Applicative
import qualified Control.Failure            as F
import           Data.Maybe
import           Text.XML.Enumerator.Cursor   (($/), ($//), (&/), (&|))
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Data.ByteString.Char8 as B

data ReceiveMessage = ReceiveMessage{ 
  rmVisibilityTimeout :: Maybe Int,
  rmAttributes :: [M.MessageAttribute],
  rmMaxNumberOfMessages :: Maybe Int,
  rmQueueName :: M.QueueName
}deriving (Show)

data Message = Message{
  mMessageId :: T.Text,
  mReceiptHandle :: M.ReceiptHandle,
  mMD5OfBody :: T.Text,
  mBody :: T.Text,
  mAttributes :: [(M.MessageAttribute,T.Text)]
} deriving(Show)

data ReceiveMessageResponse = ReceiveMessageResponse{
  rmrMessages :: [Message]
} deriving (Show)

readMessageAttribute :: F.Failure XmlException m => Cu.Cursor -> m (M.MessageAttribute,T.Text)
readMessageAttribute cursor = do
  name <- force "Missing Name" $ cursor $/ Cu.laxElement "Name" &/ Cu.content
  value <- force "Missing Value" $ cursor $/ Cu.laxElement "Value" &/ Cu.content
  return (M.parseMessageAttribute name, value)

readMessage :: Cu.Cursor -> [Message]
readMessage cursor = do
  mid :: T.Text <- force "Missing Message Id" $ cursor $// Cu.laxElement "MessageId" &/ Cu.content
  rh <- force "Missing Reciept Handle" $ cursor $// Cu.laxElement "ReceiptHandle" &/ Cu.content
  md5 <- force "Missing MD5 Signature" $ cursor $// Cu.laxElement "MD5OfBody" &/ Cu.content
  body <- force "Missing Body" $ cursor $// Cu.laxElement "Body" &/ Cu.content
  let attributes :: [(M.MessageAttribute, T.Text)] = concat $ cursor $// Cu.laxElement "Attribute" &| readMessageAttribute
  
  return Message{ mMessageId = mid, mReceiptHandle = M.ReceiptHandle rh, mMD5OfBody = md5, mBody = body, mAttributes = attributes}

formatMAttributes :: [M.MessageAttribute] -> [(B.ByteString, Maybe B.ByteString)]
formatMAttributes attrs =
  case length attrs of
    0 -> []
    1 -> [("AttributeName", Just $ B.pack $ show $ attrs !! 0)]
    _ -> zipWith (\ x y -> ((B.concat ["AttributeName.", B.pack $ show $ y]), Just $ TE.encodeUtf8 $ M.printMessageAttribute x) ) attrs [1..]

instance ResponseIteratee r ReceiveMessageResponse where
    type ResponseMetadata ReceiveMessageResponse = SqsMetadata
    responseIteratee _ = sqsXmlResponseIteratee parse
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
