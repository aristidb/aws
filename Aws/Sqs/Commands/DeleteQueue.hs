{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.DeleteQueue where

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
import           Control.Monad
import           Data.Maybe
import           Data.Time.Format
import           System.Locale
import qualified Data.Enumerator              as En
import qualified Data.Text                    as T
import qualified Text.XML.Enumerator.Cursor   as Cu
import qualified Text.XML.Enumerator.Parse    as XML
import qualified Text.XML.Enumerator.Resolved as XML
import qualified Network.HTTP.Types    as HTTP
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Char8 as B
import Debug.Trace

data DeleteQueue = DeleteQueue{
  dqQueueName :: M.QueueName 
}deriving (Show)

data DeleteQueueResponse = DeleteQueueResponse{
} deriving (Show)

instance ResponseIteratee DeleteQueueResponse where
    type ResponseMetadata DeleteQueueResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where
        parse _ = do return DeleteQueueResponse{}
          
instance SignQuery DeleteQueue  where 
    type Info DeleteQueue  = SqsInfo
    signQuery DeleteQueue {..} = sqsSignQuery SqsQuery {
                                             sqsQueueName = Just dqQueueName, 
                                             sqsQuery = [("Action", Just "DeleteQueue")]}

instance Transaction DeleteQueue DeleteQueueResponse
