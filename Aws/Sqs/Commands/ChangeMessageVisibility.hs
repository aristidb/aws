{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}

module Aws.Sqs.Commands.ChangeMessageVisibility where

import           Aws.Response
import           Aws.Sqs.Info
import           Aws.Sqs.Metadata
import qualified Aws.Sqs.Model as M
import           Aws.Sqs.Query
import           Aws.Sqs.Response
import           Aws.Signature
import           Aws.Transaction
import qualified Data.Text.Encoding           as TE
import qualified Data.ByteString.Char8        as B

data ChangeMessageVisibility = ChangeMessageVisibility {
  cmvReceiptHandle :: M.ReceiptHandle,
  cmvVisibilityTimeout :: Int,
  cmvQueueName :: M.QueueName
}deriving (Show)

data ChangeMessageVisibilityResponse = ChangeMessageVisibilityResponse{
} deriving (Show)

instance ResponseIteratee ChangeMessageVisibilityResponse where
    type ResponseMetadata ChangeMessageVisibilityResponse = SqsMetadata
    responseIteratee = sqsXmlResponseIteratee parse
      where 
        parse _ = do return ChangeMessageVisibilityResponse{}
    
instance SignQuery ChangeMessageVisibility  where 
    type Info ChangeMessageVisibility  = SqsInfo
    signQuery ChangeMessageVisibility {..} = sqsSignQuery SqsQuery { 
                                             sqsQueueName = Just cmvQueueName, 
                                             sqsQuery = [("Action", Just "ChangeMessageVisibility"), 
                                                         ("ReceiptHandle", Just $ TE.encodeUtf8 $ M.printReceiptHandle cmvReceiptHandle),
                                                         ("VisibilityTimeout", Just $ B.pack $ show cmvVisibilityTimeout)]}

instance Transaction ChangeMessageVisibility ChangeMessageVisibilityResponse
