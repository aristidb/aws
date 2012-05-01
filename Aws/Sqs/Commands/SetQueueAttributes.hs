{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.SetQueueAttributes where

import           Aws.Core
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

data SetQueueAttributes = SetQueueAttributes{
  sqaAttribute :: M.QueueAttribute,
  sqaValue :: T.Text,
  sqaQueueName :: M.QueueName 
}deriving (Show)

data SetQueueAttributesResponse = SetQueueAttributesResponse{
} deriving (Show)

instance ResponseConsumer r SetQueueAttributesResponse where
    type ResponseMetadata SetQueueAttributesResponse = SqsMetadata
    responseConsumer _ = sqsXmlResponseConsumer parse
      where 
        parse _ = do
          return SetQueueAttributesResponse {}
          
instance SignQuery SetQueueAttributes  where 
    type Info SetQueueAttributes  = SqsInfo
    signQuery SetQueueAttributes {..} = sqsSignQuery SqsQuery { 
                                             sqsQueueName = Just sqaQueueName,
                                             sqsQuery = [("Action", Just "SetQueueAttributes"), 
                                                        ("Attribute.Name", Just $ TE.encodeUtf8 $ M.printQueueAttribute sqaAttribute),
                                                        ("Attribute.Value", Just $ TE.encodeUtf8 sqaValue)]} 

instance Transaction SetQueueAttributes SetQueueAttributesResponse
