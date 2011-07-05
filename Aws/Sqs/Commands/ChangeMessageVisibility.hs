{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ChangeMessageVisibility where

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
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data ChangeMessageVisibility = ChangeMessageVisibility {
  cmvReceiptHandle :: M.ReceiptHandle,
  cmvVisibilityTimeout :: Int,
  cmvQueueName :: M.QueueName
}deriving (Show)

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse{
} deriving (Show)


cmvParse :: Cu.Cursor -> ChangeMessageVisibilityResponse
cmvParse el = do
  ChangeMessageVisibilityResponse

instance ResponseIteratee ChangeMessageVisibilityResponse where
    type ResponseMetadata ChangeMessageVisibilityResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse el = do return ChangeMessageVisibilityResponse{}
    
instance SignQuery ChangeMessageVisibility  where 
    type Info ChangeMessageVisibility  = SqsInfo
    signQuery ChangeMessageVisibility {..} = sqsSignQuery SqsQuery { 
                                             sqsQueueName = Just cmvQueueName, 
                                             sqsQuery = [("Action", Just "ChangeMessageVisibility"), 
                                                         ("ReceiptHandle", Just $ B.pack $ show cmvReceiptHandle),
                                                         ("VisibilityTimout", Just $ B.pack $ show cmvVisibilityTimeout)]}

instance Transaction ChangeMessageVisibility ChangeMessageVisibilityResponse
