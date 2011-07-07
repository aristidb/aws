{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.SetQueueAttributes where

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

data SetQueueAttributes = SetQueueAttributes{
  sqaAttribute :: M.QueueAttribute,
  sqaValue :: T.Text,
  sqaQueueName :: M.QueueName 
}deriving (Show)

data SetQueueAttributesResponse = SetQueueAttributesResponse{
} deriving (Show)

instance ResponseIteratee SetQueueAttributesResponse where
    type ResponseMetadata SetQueueAttributesResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse el = do
          return SetQueueAttributesResponse {}
          
instance SignQuery SetQueueAttributes  where 
    type Info SetQueueAttributes  = SqsInfo
    signQuery SetQueueAttributes {..} = sqsSignQuery SqsQuery { 
                                             sqsQueueName = Just sqaQueueName,
                                             sqsQuery = [("Action", Just "SetQueueAttributes"), 
                                                        ("Attribute.Name", Just $ TE.encodeUtf8 $ M.printQueueAttribute sqaAttribute),
                                                        ("Attribute.Value", Just $ TE.encodeUtf8 sqaValue)]} 

instance Transaction SetQueueAttributes SetQueueAttributesResponse
