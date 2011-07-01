{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.SendMessage where

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

data SendMessage = SendMessage{
  smMessage :: T.Text,
  smQueueName :: M.QueueName 
}deriving (Show)

data SendMessageResponse = SendMessageResponse{
  smrMD5OfMessageBody :: T.Text,
  smrMessageId :: M.MessageId
} deriving (Show)


smParse :: Cu.Cursor -> SendMessageResponse
smParse el = do SendMessageResponse { smrMD5OfMessageBody = md5, smrMessageId = mid }
  where
    md5 = head $ head $ Cu.laxElement "SendMessageResponse" &/ Cu.laxElement "SendMessageResult" &/ Cu.laxElement "MD5OfMessageBody" &| Cu.content $ el
    mid = M.MessageId $ head $ head $ Cu.laxElement "SendMessageResponse" &/ Cu.laxElement "SendMessageResult" &/ Cu.laxElement "MessageId" &| Cu.content $ el

instance SqsResponseIteratee SendMessageResponse where
    sqsResponseIteratee status headers = do doc <- XML.parseBytes XML.decodeEntities =$ XML.fromEvents
                                            let cursor = Cu.fromDocument doc
                                            return $ smParse cursor                                  
          
instance SignQuery SendMessage  where 
    type Info SendMessage  = SqsInfo
    signQuery SendMessage {..} = sqsSignQuery SqsQuery { 
                                             sqsQuery = [("Action", Just "SendMessage"), 
                                                        ("QueueName", Just $ B.pack $ M.printQueue smQueueName),
                                                        ("MessageBody", Just $ B.pack $ show smMessage )]} 

instance Transaction SendMessage (SqsResponse SendMessageResponse)



