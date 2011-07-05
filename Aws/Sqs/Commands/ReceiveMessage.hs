{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections, FlexibleContexts #-}

module Aws.Sqs.Commands.ReceiveMessage where

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
import qualified Control.Failure            as F
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

data ReceiveMessage = ReceiveMessage{ 
  rmVisibilityTimeout :: Maybe Int,
  rmAttributes :: [M.MessageAttribute],
  rmMaxNumberOfMessages :: Maybe Int,
  rmQueueName :: M.QueueName
}deriving (Show)

data Message = Message{
  mMessageId :: T.Text,
  mRecieptHandle :: M.ReceiptHandle,
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
  return ( M.parseMessageAttribute name, value)


--parseMAttributes :: Cu.Cursor -> (M.MessageAttribute, T.Text)
--parseMAttributes el =
--  (M.parseMessageAttribute $ T.concat $ T.concat $ Cu.laxElement "Name" &| Cu.content $ el, el $/ Cu.laxElement "Value" &| Cu.content)

readMessage cursor = do
  attributes <- sequence $ force "Missing Attributes" $ cursor $/ Cu.laxElement "Attribute" &| readMessageAttribute
  id <- force "Missing Message Id" $ cursor $/ Cu.laxElement "MessageId" &/ Cu.content
  rh <- force "Missing Reciept Handle" $ cursor $/ Cu.laxElement "ReceiptHandle" &/ Cu.content &| M.ReceiptHandle
  md5 <- force "Missing MD5 Signature" $ cursor $/ Cu.laxElement "MD5OfBody" &/ Cu.content
  body <- force "Missing Body" $ cursor $/ Cu.laxElement "Body" &/ Cu.content

  return Message{ mMessageId = id, mRecieptHandle = rh, mMD5OfBody = md5, mBody = body, mAttributes = attributes}


formatMAttributes :: [M.MessageAttribute] -> [HTTP.QueryItem]
formatMAttributes attrs =
  case length attrs of
    0 -> []
    1 -> [("AttributeName", Just $ B.pack $ show $ attrs !! 0)]
    _ -> zipWith (\ x y -> ((B.concat ["AttributeName.", B.pack $ show $ y]), Just $ B.pack $ M.printMessageAttribute x) ) attrs [1..]

instance ResponseIteratee ReceiveMessageResponse where
    type ResponseMetadata ReceiveMessageResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse el = do
          let messages = el $// Cu.laxElement "Message" &| readMessage
          ReceiveMessageResponse{ rmrMessages = messages }
          
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
