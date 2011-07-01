{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ReceiveMessage where

import           Aws.Response
import           Aws.Sqs.Error
import           Aws.Sqs.Info
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

parseMAttributes :: Cu.Cursor -> (M.MessageAttribute, T.Text)
parseMAttributes el =
  (M.parseMessageAttribute $ head $ el $/ Cu.laxElement "Name" &/ Cu.content, 
   head $ el $/ Cu.laxElement "Value" &/ Cu.content)


mParse :: Cu.Cursor -> Message
mParse el = do
    Message{
      mMessageId = id,
      mRecieptHandle = rh,
      mMD5OfBody = md5,
      mBody = body,
      mAttributes = attributes}
  where
    id = head $ head $ Cu.laxElement "MessageId" &| Cu.content $ el
    rh = M.ReceiptHandle $ head $ head $ Cu.laxElement "ReceiptHandle" &| Cu.content $ el
    md5 = head $ head $Cu.laxElement "MD5OfBody" &| Cu.content $ el
    body = head $ head $ Cu.laxElement "Body" &| Cu.content $el
    attributes = Cu.laxElement "Attribute" &| parseMAttributes $ el

rmParse :: Cu.Cursor -> ReceiveMessageResponse
rmParse el = do
  let messages = Cu.laxElement "ReceiveMessageResponse" &/ Cu.laxElement "ReceiveMessageResult" &/ Cu.laxElement "Message" &| mParse $ el
  ReceiveMessageResponse{ rmrMessages = messages }

formatMAttributes :: [M.MessageAttribute] -> [HTTP.QueryItem]
formatMAttributes attrs =
  case length attrs of
    0 -> []
    1 -> [("AttributeName", Just $ B.pack $ show $ attrs !! 0)]
    _ -> zipWith (\ x y -> ((B.concat ["AttributeName.", B.pack $ show $ y]), Just $ B.pack $ M.printMessageAttribute x) ) attrs [1..]

instance SqsResponseIteratee ReceiveMessageResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = Cu.fromDocument doc
                                            return $ rmParse cursor                                  
          
instance SignQuery ReceiveMessage  where 
    type Info ReceiveMessage  = SqsInfo
    signQuery ReceiveMessage {..} = sqsSignQuery SqsQuery { 
                                             sqsQuery = [("Action", Just "ReceiveMessage"), 
                                                         ("QueueName", Just $ B.pack $ M.printQueue rmQueueName)] ++
                                                         catMaybes[("VisibilityTimeout",) <$> case rmVisibilityTimeout of
                                                                                                Just x -> Just $ Just $ B.pack $ show x
                                                                                                Nothing -> Nothing,
                                                                   ("MaxNumberOfMessages",) <$> case rmMaxNumberOfMessages of
                                                                                                  Just x -> Just $ Just $ B.pack $ show x
                                                                                                  Nothing -> Nothing]
                                                         ++ formatMAttributes rmAttributes}

instance Transaction ReceiveMessage (SqsResponse ReceiveMessageResponse)
